// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.builder;

import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

import wybs.lang.Build;
import wybs.lang.NameID;
import wybs.lang.NameResolver;
import wybs.lang.NameResolver.ResolutionError;
import wybs.util.AbstractCompilationUnit.Tuple;
import wyc.lang.WhileyFile;
import wyc.lang.WhileyFile.Decl;
import wyc.util.AbstractConsumer;
import wyc.util.AbstractFunction;
import wybs.lang.Build.Graph;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Path;
import wyfs.lang.Path.Entry;
import wyfs.lang.Path.Root;
import wyfs.util.Trie;
import wyil.type.TypeSystem;

import static wyc.lang.WhileyFile.*;

import wyjc.core.JavaFile;
import wyjc.util.JavaCodeGenerator;

public class JavaCompileTask extends AbstractFunction<JavaFile.Class, JavaFile.Term> implements Build.Task {
	/**
	 * The master project for identifying all resources available to the builder.
	 * This includes all modules declared in the project being verified and/or
	 * defined in external resources (e.g. jar files).
	 */
	protected final Build.Project project;

	/**
	 * The type system is useful for managing nominal types and converting them into
	 * their underlying types.
	 */
	protected final TypeSystem typeSystem;

	/**
	 * For logging information.
	 */
	private Logger logger = Logger.NULL;

	private ArrayList<Pair<Type, Type>> coercions;

	public JavaCompileTask(Build.Project project) {
		this.project = project;
		this.typeSystem = new TypeSystem(project);
	}

	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	@Override
	public Build.Project project() {
		return project;
	}

	@Override
	public Set<Entry<?>> build(Collection<Pair<Entry<?>, Root>> delta, Graph graph) throws IOException {
		Runtime runtime = Runtime.getRuntime();
		long start = System.currentTimeMillis();
		long memory = runtime.freeMemory();

		// ========================================================================
		// Translate files
		// ========================================================================
		HashSet<Path.Entry<?>> generatedFiles = new HashSet<>();

		for (Pair<Path.Entry<?>, Path.Root> p : delta) {
			Path.Root dst = p.second();
			Path.Entry<WhileyFile> source = (Path.Entry<WhileyFile>) p.first();
			Path.Entry<JavaFile> target = dst.create(source.id(), JavaFile.ContentType);
			graph.registerDerivation(source, target);
			generatedFiles.add(target);

			// Construct the file
			JavaFile contents = build(source, target);

			// Write class file into its destination
			target.write(contents);
		}

		// ========================================================================
		// Done
		// ========================================================================

		long endTime = System.currentTimeMillis();
		logger.logTimedMessage("Wyil => Java: compiled " + delta.size() + " file(s)", endTime - start,
				memory - runtime.freeMemory());

		return generatedFiles;
	}

	private JavaFile build(Path.Entry<WhileyFile> source, Path.Entry<JavaFile> target) throws IOException {
		// Read the source file, which forces it to be parsed, etc.
		WhileyFile wf = source.read();
		// Reset the list of coercions
		coercions = new ArrayList<>();
		// Create an (empty) output file to contains the generated Java source
		// code.
		JavaFile jf = new JavaFile(target);

		// Add package declaration?

		// Add imports
		jf.getDeclarations().add(new JavaFile.Import("java", "math", "BigInteger"));
		jf.getDeclarations().add(new JavaFile.Import("java", "util", "Arrays"));
		jf.getDeclarations().add(new JavaFile.Import("java", "util", "function", "Function"));
		jf.getDeclarations().add(new JavaFile.Import("wyjc", "runtime", "Wy"));

		String className = wf.getEntry().id().last();
		JavaFile.Class jcd = new JavaFile.Class(className);
		jcd.getModifiers().add(JavaFile.Modifier.PUBLIC);
		jcd.getModifiers().add(JavaFile.Modifier.FINAL);
		// Translate all declarations
		visitWhileyFile(wf, jcd);
		// Add all required coercions
		translateCoercions(coercions, jcd);
		// Add launcher (if appropriate)
		addMainLauncher(wf, jcd);
		// Done
		jf.getDeclarations().add(jcd);
		return jf;
	}

	@Override
	public JavaFile.Term visitDeclaration(Decl decl, JavaFile.Class parent) {
		JavaFile.Declaration jDecl = (JavaFile.Declaration) super.visitDeclaration(decl, parent);
		if (jDecl != null) {
			parent.getDeclarations().add(jDecl);
		}
		return null;
	}

	@Override
	public JavaFile.Declaration visitStaticVariable(Decl.StaticVariable decl, JavaFile.Class parent) {
		JavaFile.Type type = visitType(decl.getType(), parent);
		String name = decl.getName().toString();
		JavaFile.Term initialiser = null;
		if (decl.hasInitialiser()) {
			initialiser = visitExpression(decl.getInitialiser(), parent);
		}
		JavaFile.Field jDecl = new JavaFile.Field(type, name, initialiser);
		jDecl.getModifiers().add(JavaFile.Modifier.STATIC);
		return jDecl;
	}

	@Override
	public JavaFile.Declaration visitType(Decl.Type decl, JavaFile.Class parent) {
		Type type = decl.getType();
		// Type aliases do not exist in Java. Instead, we have only their type
		// invariants.
		return null;
	}

	@Override
	public JavaFile.Declaration visitFunctionOrMethod(Decl.FunctionOrMethod decl, JavaFile.Class parent) {
		Type.Callable sig = decl.getType();
		JavaFile.Type returnType = translateReturnTypes(sig.getReturns());
		//
		JavaFile.Method method = new JavaFile.Method(decl.getName().toString(), returnType);
		method.getModifiers().addAll(translateModifiers(decl.getModifiers()));
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		//
		List<JavaFile.VariableDeclaration> parameters = visitVariableDeclarations(decl.getParameters());
		method.getParameters().addAll(parameters);
		//
		// FIXME: should provide support for checking preconditions /
		// postconditions here. These can be implemented as runtime checks.
		//
		method.setBody(visitBlock(decl.getBody(), parent));
		return method;
	}

	private List<JavaFile.VariableDeclaration> visitVariableDeclarations(Tuple<Decl.Variable> decls) {
		List<JavaFile.VariableDeclaration> result = new ArrayList<>();
		for (int i = 0; i != decls.size(); ++i) {
			Decl.Variable pd = decls.get(i);
			JavaFile.Type pt = visitType(pd.getType(), null);
			result.add(new JavaFile.VariableDeclaration(pt, pd.getName().toString(), null));
		}
		return result;
	}

	@Override
	public JavaFile.Term visitVariable(Decl.Variable stmt, JavaFile.Class parent) {
		JavaFile.Type type = visitType(stmt.getType(), parent);
		JavaFile.Term initialiser = null;
		if (stmt.hasInitialiser()) {
			initialiser = visitExpression(stmt.getInitialiser(), parent);
		}
		return new JavaFile.VariableDeclaration(type, stmt.getName().get(), initialiser);
	}

	@Override
	public JavaFile.Block visitBlock(Stmt.Block block, JavaFile.Class parent) {
		JavaFile.Block jblock = new JavaFile.Block();
		for (int i = 0; i != block.size(); ++i) {
			JavaFile.Term jterm = visitStatement(block.get(i), parent);
			jblock.getTerms().add(jterm);
		}
		return jblock;
	}

	@Override
	public JavaFile.Term visitAssert(Stmt.Assert stmt, JavaFile.Class parent) {
		JavaFile.Term operand = visitExpression(stmt.getCondition(), parent);
		return new JavaFile.Assert(operand);
	}

	@Override
	public JavaFile.Term visitAssign(Stmt.Assign stmt, JavaFile.Class parent) {
		Tuple<LVal> lvals = stmt.getLeftHandSide();
		List<JavaFile.Term> rvals = visitExpressions(stmt.getRightHandSide());
		JavaFile.Block block = new JavaFile.Block();
		for (int i = 0; i != lvals.size(); ++i) {
			block.getTerms().add(visitLVal(lvals.get(i), rvals.get(i)));
		}
		if (block.getTerms().size() == 1) {
			return block.getTerms().get(0);
		} else {
			return block;
		}
	}

	@Override
	public JavaFile.Term visitAssume(Stmt.Assume stmt, JavaFile.Class parent) {
		JavaFile.Term operand = visitExpression(stmt.getCondition(), parent);
		return new JavaFile.Assert(operand);
	}

	@Override
	public JavaFile.Term visitBreak(Stmt.Break stmt, JavaFile.Class parent) {
		return new JavaFile.Break();
	}

	@Override
	public JavaFile.Term visitContinue(Stmt.Continue stmt, JavaFile.Class parent) {
		return new JavaFile.Continue();
	}

	@Override
	public JavaFile.Term visitDebug(Stmt.Debug stmt, JavaFile.Class parent) {
		JavaFile.Term arg = visitExpression(stmt.getOperand(), parent);
		return new JavaFile.Invoke(null, new String[] { "Wy", "debug" }, arg);
	}

	@Override
	public JavaFile.Term visitDoWhile(Stmt.DoWhile stmt, JavaFile.Class parent) {
		JavaFile.Term condition = visitExpression(stmt.getCondition(), parent);
		JavaFile.Block body = visitBlock(stmt.getBody(), parent);
		return new JavaFile.DoWhile(body, condition);
	}

	@Override
	public JavaFile.Term visitIfElse(Stmt.IfElse stmt, JavaFile.Class parent) {
		JavaFile.Term operand = visitExpression(stmt.getCondition(), parent);
		JavaFile.Block trueBranch = visitBlock(stmt.getTrueBranch(), parent);
		JavaFile.Block falseBranch = null;
		if (stmt.hasFalseBranch()) {
			falseBranch = visitBlock(stmt.getFalseBranch(), parent);
		}
		return new JavaFile.If(operand, trueBranch, falseBranch);
	}

	@Override
	public JavaFile.Term visitReturn(Stmt.Return stmt, JavaFile.Class parent) {
		JavaFile.Term initialiser = null;
		Tuple<Expr> returns = stmt.getReturns();
		if (returns.size() > 0) {
			List<JavaFile.Term> tmp = visitExpressions(returns);
			if (tmp.size() > 1) {
				throw new RuntimeException("need to implement multiple returns");
			}
			initialiser = tmp.get(0);
		}
		return new JavaFile.Return(initialiser);
	}

	@Override
	public JavaFile.Term visitSwitch(Stmt.Switch stmt, JavaFile.Class parent) {
		Tuple<Stmt.Case> cases = stmt.getCases();
		JavaFile.Term[][] groups = new JavaFile.Term[cases.size()][];
		// Translate all case conditions and see whether can use a switch or not.
		boolean canSwitch = true;
		for (int i = 0; i != groups.length; ++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			JavaFile.Term[] terms = new JavaFile.Term[conditions.size()];
			for (int j = 0; j != terms.length; ++j) {
				terms[j] = visitExpression(conditions.get(j), parent);
				canSwitch &= (terms[j] instanceof JavaFile.Constant);
			}
			groups[i] = terms;
		}
		//
		if (canSwitch) {
			// Yes, we can switch
			return visitSwitchAsSwitch(stmt, groups, parent);
		} else {
			return visitSwitchAsIfChain(stmt, groups, parent);
		}
	}

	public JavaFile.Term visitSwitchAsSwitch(Stmt.Switch stmt, JavaFile.Term[][] groups, JavaFile.Class parent) {
		JavaFile.Term condition = visitExpression(stmt.getCondition(), parent);
		Tuple<Stmt.Case> cases = stmt.getCases();
		ArrayList<JavaFile.Case> jCases = new ArrayList<>();
		for (int i = 0; i != cases.size(); ++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			for (int j = 0; j != conditions.size(); ++j) {
				JavaFile.Constant label = (JavaFile.Constant) groups[i][j];
				JavaFile.Block body = visitBlock(c.getBlock(), parent);
				jCases.add(new JavaFile.Case(label, body));
			}
		}
		return new JavaFile.Switch(condition, jCases);
	}

	public JavaFile.Term visitSwitchAsIfChain(Stmt.Switch stmt, JavaFile.Term[][] groups, JavaFile.Class parent) {
		// FIXME: this obviously doesn't work --- need to figure out a proper temporary
		// variable name.
		String tmpVar = "tmp";
		ArrayList<JavaFile.Term> stmts = new ArrayList<>();
		Tuple<Stmt.Case> cases = stmt.getCases();
		JavaFile.Term condition = visitExpression(stmt.getCondition(), parent);
		JavaFile.Type type = visitType(stmt.getCondition().getType(), parent);
		stmts.add(new JavaFile.VariableDeclaration(type, tmpVar, condition));
		//
		ArrayList<JavaFile.IfElse.Case> ifconditions = new ArrayList<>();
		for (int i = 0; i != cases.size(); ++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			JavaFile.Term cas = null;
			for (int j = 0; j != conditions.size(); ++j) {
				// Translate the value itself
				JavaFile.Term t = visitExpression(conditions.get(j), parent);
				// Convert it into an equality
				t = new JavaFile.Invoke(new JavaFile.VariableAccess(tmpVar), "equals", t);
				cas = (j == 0) ? t : new JavaFile.Operator(JavaFile.Operator.Kind.OR, cas, t);
			}
			JavaFile.Block body = visitBlock(c.getBlock(), parent);
			ifconditions.add(new JavaFile.IfElse.Case(cas, body));
		}
		stmts.add(new JavaFile.IfElse(ifconditions));
		return new JavaFile.Block(stmts);
	}

	@Override
	public JavaFile.Term visitWhile(Stmt.While stmt, JavaFile.Class parent) {
		JavaFile.Term condition = visitExpression(stmt.getCondition(), parent);
		JavaFile.Block body = visitBlock(stmt.getBody(), parent);
		return new JavaFile.While(condition, body);
	}

	private JavaFile.Term visitLVal(LVal lval, JavaFile.Term rval) {
		if (lval instanceof Expr.VariableAccess) {
			return visitVariableLVal((Expr.VariableAccess) lval, rval);
		} else if (lval instanceof Expr.RecordAccess) {
			return visitRecordLVal((Expr.RecordAccess) lval, rval);
		} else if (lval instanceof Expr.ArrayAccess) {
			return visitArrayLVal((Expr.ArrayAccess) lval, rval);
		} else {
			return visitExpression(lval, null);
		}
	}

	private JavaFile.Term visitVariableLVal(Expr.VariableAccess lval, JavaFile.Term rval) {
		Decl.Variable vd = lval.getVariableDeclaration();
		return new JavaFile.Assignment(new JavaFile.VariableAccess(vd.getName().toString()), rval);
	}

	private JavaFile.Term visitRecordLVal(Expr.RecordAccess lval, JavaFile.Term rval) {
		JavaFile.Term src = visitExpression(lval.getOperand(), null);
		return new JavaFile.Invoke(src, "put", new JavaFile.Constant(lval.getField().toString()), rval);
	}

	private JavaFile.Term visitArrayLVal(Expr.ArrayAccess lval, JavaFile.Term rval) {
		JavaFile.Term src = visitExpression(lval.getFirstOperand(), null);
		JavaFile.Term index = visitExpression(lval.getSecondOperand(), null);
		return new JavaFile.Assignment(new JavaFile.ArrayAccess(src, toInt(index, Type.Int)), rval);
	}

	private List<JavaFile.Term> visitExpressions(Tuple<Expr> exprs) {
		List<JavaFile.Term> arguments = new ArrayList<>();
		for (int i = 0; i != exprs.size(); ++i) {
			arguments.add(visitExpression(exprs.get(i), null));
		}
		return arguments;
	}

	@Override
	public JavaFile.Term visitArrayAccess(Expr.ArrayAccess expr, JavaFile.Class parent) {
		JavaFile.Term src = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term index = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.ArrayAccess(src, toInt(index, Type.Int));
	}

	@Override
	public JavaFile.Term visitArrayGenerator(Expr.ArrayGenerator expr, JavaFile.Class parent) {
		throw new IllegalArgumentException("IMPLEMENT ME");
	}

	@Override
	public JavaFile.Term visitArrayInitialiser(Expr.ArrayInitialiser expr, JavaFile.Class parent) {
		Tuple<Expr> operands = expr.getOperands();
		List<JavaFile.Term> children = new ArrayList<>();
		for (int i = 0; i != operands.size(); ++i) {
			children.add(visitExpression(operands.get(i), parent));
		}
		JavaFile.Array type = (JavaFile.Array) visitType(expr.getType(), null);
		return new JavaFile.NewArray(type, null, children);
	}

	@Override
	public JavaFile.Term visitArrayLength(Expr.ArrayLength expr, JavaFile.Class parent) {
		JavaFile.Term src = visitExpression(expr.getOperand(), parent);
		// FIXME: converting the array length to a big integer is a temporary
		// fix. It works around the fact that the Whiley compiler types the
		// return of an array length expression as just "int", when in fact it
		// should be: "usize".
		return toBigInteger(new JavaFile.FieldAccess(src, "length"), Type.Int);
	}

	@Override
	public JavaFile.Term visitBitwiseComplement(Expr.BitwiseComplement expr, JavaFile.Class parent) {
		JavaFile.Term t = translateOperator(JavaFile.Operator.Kind.BITWISEINVERT, expr.getOperand());
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitBitwiseAnd(Expr.BitwiseAnd expr, JavaFile.Class parent) {
		JavaFile.Term t = translateOperator(JavaFile.Operator.Kind.BITWISEAND, expr.getOperands());
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitBitwiseOr(Expr.BitwiseOr expr, JavaFile.Class parent) {
		JavaFile.Term t = translateOperator(JavaFile.Operator.Kind.BITWISEOR, expr.getOperands());
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitBitwiseXor(Expr.BitwiseXor expr, JavaFile.Class parent) {
		JavaFile.Term t = translateOperator(JavaFile.Operator.Kind.BITWISEXOR, expr.getOperands());
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitBitwiseShiftLeft(Expr.BitwiseShiftLeft expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term t = new JavaFile.Operator(JavaFile.Operator.Kind.LEFTSHIFT, lhs, toInt(rhs, Type.Int));
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitBitwiseShiftRight(Expr.BitwiseShiftRight expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term t = new JavaFile.Operator(JavaFile.Operator.Kind.RIGHTSHIFT, lhs, toInt(rhs, Type.Int));
		return new JavaFile.Cast(JavaFile.BYTE, t);
	}

	@Override
	public JavaFile.Term visitCast(Expr.Cast expr, JavaFile.Class parent) {
		return registerCoercion(expr.getType(), expr.getOperand(), parent);
	}

	@Override
	public JavaFile.Term visitConstant(Expr.Constant expr, JavaFile.Class parent) {
		Value c = expr.getValue();
		// Type type = c.type();
		Object value;
		if (c instanceof Value.Null) {
			value = null;
		} else if (c instanceof Value.Bool) {
			Value.Bool bc = (Value.Bool) c;
			value = bc.get();
		} else if (c instanceof Value.Byte) {
			Value.Byte bc = (Value.Byte) c;
			value = bc.get();
		} else if (c instanceof Value.Int) {
			Value.Int bc = (Value.Int) c;
			return translateUnboundIntegerConstant(bc.get());
		} else {
			return translateUtf8Constant(((Value.UTF8) c).get());
		}
		return new JavaFile.Constant(value);
	}

	@Override
	public JavaFile.Term visitEqual(Expr.Equal expr, JavaFile.Class parent) {
		if (isDynamicallySized(expr.getFirstOperand().getType())) {
			JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
			JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
			return new JavaFile.Invoke(null, new String[] { "Wy", "equals" }, lhs, rhs);
		} else {
			return translateOperator(JavaFile.Operator.Kind.EQ, expr.getFirstOperand(), expr.getSecondOperand());
		}
	}

	@Override
	public JavaFile.Term visitNotEqual(Expr.NotEqual expr, JavaFile.Class parent) {
		if (isDynamicallySized(expr.getFirstOperand().getType())) {
			JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
			JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
			JavaFile.Term eq = new JavaFile.Invoke(null, new String[] { "Wy", "equals" }, lhs, rhs);
			return new JavaFile.Operator(JavaFile.Operator.Kind.NOT, eq);
		} else {
			return translateOperator(JavaFile.Operator.Kind.NEQ, expr.getFirstOperand(), expr.getSecondOperand());
		}
	}

	@Override
	public JavaFile.Term visitRecordAccess(Expr.RecordAccess expr, JavaFile.Class parent) {
		JavaFile.Term source = visitExpression(expr.getOperand(), parent);
		// FIXME: this is automatically assuming an indirect look up. In fact, can
		// perform a direct look up in some cases.
		return new JavaFile.Invoke(source, "get", new JavaFile.Constant(expr.getField().toString()));
	}

	@Override
	public JavaFile.Term visitIndirectInvoke(Expr.IndirectInvoke expr, JavaFile.Class parent) {
		JavaFile.Term receiver = visitExpression(expr.getSource(), parent);
		List<JavaFile.Term> arguments = visitExpressions(expr.getArguments());
		return new JavaFile.Invoke(receiver, "apply", arguments);
	}

	@Override
	public JavaFile.Term visitIntegerNegation(Expr.IntegerNegation expr, JavaFile.Class parent) {
		JavaFile.Term jTerm = visitExpression(expr.getOperand(), parent);
		return new JavaFile.Invoke(jTerm, "negate");
	}

	@Override
	public JavaFile.Term visitIntegerAddition(Expr.IntegerAddition expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.Invoke(lhs, "add", rhs);
	}

	@Override
	public JavaFile.Term visitIntegerSubtraction(Expr.IntegerSubtraction expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.Invoke(lhs, "subtract", rhs);
	}

	@Override
	public JavaFile.Term visitIntegerMultiplication(Expr.IntegerMultiplication expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.Invoke(lhs, "multiply", rhs);
	}

	@Override
	public JavaFile.Term visitIntegerDivision(Expr.IntegerDivision expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.Invoke(lhs, "divide", rhs);
	}

	@Override
	public JavaFile.Term visitIntegerRemainder(Expr.IntegerRemainder expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		return new JavaFile.Invoke(lhs, "remainder", rhs);
	}

	@Override
	public JavaFile.Term visitIntegerLessThan(Expr.IntegerLessThan expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term cmp = new JavaFile.Invoke(lhs, "compareTo", rhs);
		return new JavaFile.Operator(JavaFile.Operator.Kind.LT, cmp, new JavaFile.Constant(0));
	}

	@Override
	public JavaFile.Term visitIntegerLessThanOrEqual(Expr.IntegerLessThanOrEqual expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term cmp = new JavaFile.Invoke(lhs, "compareTo", rhs);
		return new JavaFile.Operator(JavaFile.Operator.Kind.LTEQ, cmp, new JavaFile.Constant(0));
	}

	@Override
	public JavaFile.Term visitIntegerGreaterThan(Expr.IntegerGreaterThan expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term cmp = new JavaFile.Invoke(lhs, "compareTo", rhs);
		return new JavaFile.Operator(JavaFile.Operator.Kind.GT, cmp, new JavaFile.Constant(0));
	}

	@Override
	public JavaFile.Term visitIntegerGreaterThanOrEqual(Expr.IntegerGreaterThanOrEqual expr, JavaFile.Class parent) {
		JavaFile.Term lhs = visitExpression(expr.getFirstOperand(), parent);
		JavaFile.Term rhs = visitExpression(expr.getSecondOperand(), parent);
		JavaFile.Term cmp = new JavaFile.Invoke(lhs, "compareTo", rhs);
		return new JavaFile.Operator(JavaFile.Operator.Kind.GTEQ, cmp, new JavaFile.Constant(0));
	}

	@Override
	public JavaFile.Term visitLogicalNot(Expr.LogicalNot expr, JavaFile.Class parent) {
		return translateOperator(JavaFile.Operator.Kind.NOT, expr.getOperand());
	}

	@Override
	public JavaFile.Term visitLogicalAnd(Expr.LogicalAnd expr, JavaFile.Class parent) {
		return translateOperator(JavaFile.Operator.Kind.AND, expr.getOperands());
	}

	@Override
	public JavaFile.Term visitLogicalOr(Expr.LogicalOr expr, JavaFile.Class parent) {
		return translateOperator(JavaFile.Operator.Kind.OR, expr.getOperands());
	}

	@Override
	public JavaFile.Term visitLogicalIff(Expr.LogicalIff expr, JavaFile.Class parent) {
		return translateOperator(JavaFile.Operator.Kind.EQ, expr.getFirstOperand(), expr.getSecondOperand());
	}

	@Override
	public JavaFile.Term visitLogicalImplication(Expr.LogicalImplication expr, JavaFile.Class parent) {
		// Java doesn't have an explicit implication operator.
		return translateOperator(JavaFile.Operator.Kind.OR, new Expr.LogicalNot(expr.getFirstOperand()),
				expr.getSecondOperand());
	}

	@Override
	public JavaFile.Term visitInvoke(Expr.Invoke expr, JavaFile.Class parent) {
		List<String> path = new ArrayList<>();
		Tuple<Expr> operands = expr.getOperands();
		List<JavaFile.Term> arguments = new ArrayList<>();
		// FIXME: need to do better here
		path.add(expr.getName().toNameID().name());
		for (int i = 0; i != operands.size(); ++i) {
			arguments.add(visitExpression(operands.get(i), parent));
		}
		return new JavaFile.Invoke(null, path, arguments);
	}

	@Override
	public JavaFile.Term visitIs(Expr.Is expr, JavaFile.Class parent) {
		return new JavaFile.Constant(false);
	}

	@Override
	public JavaFile.Term visitLambdaAccess(Expr.LambdaAccess expr, JavaFile.Class parent) {
		Tuple<Type> types = expr.getSignature().getParameters();
		List<JavaFile.VariableDeclaration> parameters = new ArrayList<>();
		List<JavaFile.Term> arguments = new ArrayList<>();
		for (int i = 0; i != types.size(); ++i) {
			String name = "p" + i;
			JavaFile.Type type = visitType(types.get(i), parent);
			parameters.add(new JavaFile.VariableDeclaration(type, name));
			arguments.add(new JavaFile.VariableAccess(name));
		}
		List<String> path = new ArrayList<>();
		// FIXME: need to do better here
		path.add(expr.getName().toNameID().name());
		JavaFile.Term body = new JavaFile.Invoke(null, path, arguments);
		return new JavaFile.Lambda(parameters, body);
	}

	@SuppressWarnings("unchecked")
	@Override
	public JavaFile.Term visitLambda(Decl.Lambda expr, JavaFile.Class parent) {

		return null;
	}

	@Override
	public JavaFile.Term visitRecordInitialiser(Expr.RecordInitialiser expr, JavaFile.Class parent) {
		Type type = expr.getType(); // declared type (if applicable)
		// Construct the appropriate record now
		Tuple<Expr> operands = expr.getOperands();
		ArrayList<JavaFile.Term> parameters = new ArrayList<>();
		parameters.add(translateRecordSchema(type));
		for (int i = 0; i != operands.size(); ++i) {
			Expr operand = operands.get(i);
			parameters.add(visitExpression(operand, parent));
		}
		return new JavaFile.New(WHILEY_RECORD, parameters);
	}

	@Override
	public JavaFile.Term visitUniversalQuantifier(Expr.UniversalQuantifier c, JavaFile.Class parent) {
		return translateQuantifier(c, parent);
	}

	@Override
	public JavaFile.Term visitExistentialQuantifier(Expr.ExistentialQuantifier c, JavaFile.Class parent) {
		return translateQuantifier(c, parent);
	}

	@Override
	public JavaFile.Term visitVariableAccess(Expr.VariableAccess expr, JavaFile.Class parent) {
		Decl.Variable vd = expr.getVariableDeclaration();
		JavaFile.Term t = new JavaFile.VariableAccess(vd.getName().toString());
		JavaFile.Type type = visitType(expr.getType(), parent);
		if (expr.getOpcode() == EXPR_variablecopy && !isCopyable(expr.getType())) {
			// Since this type is not copyable, we need to clone it to ensure
			// that ownership is properly preserved.
			t = new JavaFile.Invoke(t, "clone");
		}
		return t;
	}

	@Override
	public JavaFile.Term visitStaticVariableAccess(Expr.StaticVariableAccess expr, JavaFile.Class parent) {
		// FIXME: name translation broken
		JavaFile.Term t = new JavaFile.VariableAccess(expr.getName().toString());
		JavaFile.Type type = visitType(expr.getType(), parent);
		if (expr.getOpcode() == EXPR_variablecopy && !isCopyable(expr.getType())) {
			// Since this type is not copyable, we need to clone it to ensure
			// that ownership is properly preserved.
			t = new JavaFile.Invoke(t, "clone");
		}
		return t;
	}

	@Override
	public JavaFile.Type visitType(Type type, JavaFile.Class parent) {
		return (JavaFile.Type) super.visitType(type, parent);
	}

	@Override
	public JavaFile.Type visitAny(Type.Any type, JavaFile.Class parent) {
		return JAVA_LANG_OBJECT;
	}

	@Override
	public JavaFile.Type visitBool(Type.Bool type, JavaFile.Class parent) {
		return JavaFile.BOOLEAN;
	}

	@Override
	public JavaFile.Type visitByte(Type.Byte type, JavaFile.Class parent) {
		return JavaFile.BYTE;
	}

	@Override
	public JavaFile.Type visitInt(Type.Int type, JavaFile.Class parent) {
		// FIXME: want to optimise this
		return JAVA_MATH_BIGINTEGER;
	}

	@Override
	public JavaFile.Type visitArray(Type.Array type, JavaFile.Class parent) {
		return new JavaFile.Array(visitType(type.getElement(), parent));
	}

	@Override
	public JavaFile.Type visitNominal(Type.Nominal type, JavaFile.Class parent) {
		try {
			Decl.Type decl = typeSystem.resolveExactly(type.getName(), Decl.Type.class);
			return visitType(decl.getType(), parent);
		} catch (NameResolver.ResolutionError e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public JavaFile.Type visitRecord(Type.Record type, JavaFile.Class parent) {
		return WHILEY_RECORD;
	}

	@Override
	public JavaFile.Type visitCallable(Type.Callable type, JavaFile.Class parent) {
		// FIXME: this is a hack for now
		return new JavaFile.Reference("Function");
	}

	@Override
	public JavaFile.Type visitUnion(Type.Union type, JavaFile.Class parent) {
		return JAVA_LANG_OBJECT;
	}

	private JavaFile.Type[] visitTypes(Type... types) {
		JavaFile.Type[] rs = new JavaFile.Type[types.length];
		for (int i = 0; i != types.length; ++i) {
			rs[i] = visitType(types[i], null);
		}
		return rs;
	}

	// ===================================================================================
	// Main Launcher
	// ===================================================================================

	/**
	 * The main launcher is responsible for invoking the Java main(String[])
	 * function. The launcher is only added if there is an exported public method
	 * main which accepts the arguments ascii::string[].
	 *
	 * @param jcs
	 * @param wf
	 */
	private void addMainLauncher(WhileyFile wf, JavaFile.Class jcs) {
		Decl.Method mainMethod = findMainMethod(wf);
		//
		if (mainMethod != null) {
			JavaFile.Method meth = new JavaFile.Method("main", JavaFile.VOID);
			// Set modifiers to static public
			meth.getModifiers().add(JavaFile.Modifier.PUBLIC);
			meth.getModifiers().add(JavaFile.Modifier.STATIC);
			// Add parameter String[] args
			JavaFile.VariableDeclaration args = new JavaFile.VariableDeclaration(JAVA_LANG_STRING_ARRAY, "args");
			meth.getParameters().add(args);
			ArrayList<JavaFile.Term> body = new ArrayList<>();
			JavaFile.Term conversion = new JavaFile.Invoke(null,new String[] {"Wy","toAsciiStrings"},new JavaFile.VariableAccess("args"));
			body.add(new JavaFile.Invoke(null, "main", conversion));
			meth.setBody(new JavaFile.Block(body));
			jcs.getDeclarations().add(meth);
		}
	}

	private Decl.Method findMainMethod(WhileyFile wf) {
		Tuple<Decl> declarations = wf.getDeclarations();
		for (int i = 0; i != declarations.size(); ++i) {
			Decl decl = declarations.get(i);
			if (decl instanceof Decl.Method) {
				Decl.Method meth = (Decl.Method) decl;
				if (isMainMethod(meth)) {
					return meth;
				}
			}
		}
		return null;
	}

	private static final Type.Array ARGS_ARR_T = new Type.Array(
			new Type.Nominal(new Name(new Identifier("ascii"), new Identifier("string"))));
	private static final Type.Method MAIN_METHOD_T = new Type.Method(new Tuple<Type>(ARGS_ARR_T), new Tuple<>(),
			new Tuple<>(), new Tuple<>());

	private boolean isMainMethod(Decl.Method method) {
		Tuple<Modifier> modifiers = method.getModifiers();
		//
		if (method.getName().get().equals("main") && modifiers.match(Modifier.Export.class) != null
				&& modifiers.match(Modifier.Public.class) != null) {
			// We have an appropriate named method which is public and exported. Now, does
			// it have the right type?
			Type.Method type = method.getType();
			return type.equals(MAIN_METHOD_T);
		}
		return false;
	}

	// ===================================================================================
	// Translation Helpers
	// ===================================================================================

	private JavaFile.Term translateQuantifier(Expr.Quantifier expr, JavaFile.Class parent) {
		String name = "block$" + expr.getIndex();
		// Determine all used variables within the quantifier
		HashSet<Decl.Variable> usedVariables = new HashSet<>();
		usedVariableExtractor.visitExpression(expr, usedVariables);
		// Construct the method in which we will execute this quantifier.
		JavaFile.Method method = new JavaFile.Method(name, JavaFile.BOOLEAN);
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		method.getModifiers().add(JavaFile.Modifier.PRIVATE);
		// Construct appropriate parameters / arguments
		JavaFile.Term[] arguments = new JavaFile.Term[usedVariables.size()];
		int argIndex = 0;
		for (Decl.Variable vd : usedVariables) {
			JavaFile.Type pt = visitType(vd.getType(), null);
			method.getParameters().add(new JavaFile.VariableDeclaration(pt, vd.getName().toString(), null));
			arguments[argIndex++] = new JavaFile.VariableAccess(vd.getName().toString());
		}
		// Translate each of the parameters into a for loop
		List<JavaFile.Term> terms = new ArrayList<>();
		terms.add(translateQuantifierBody(expr, 0, parent));
		// Add final return statement
		terms.add(new JavaFile.Return(new JavaFile.Constant(expr instanceof Expr.UniversalQuantifier)));
		// Create method body and add to enclosing class
		method.setBody(new JavaFile.Block(terms));
		parent.getDeclarations().add(method);
		// Finally, return an invocation to this
		return new JavaFile.Invoke(null, name, arguments);
	}

	private JavaFile.Term translateQuantifierBody(Expr.Quantifier expr, int i, JavaFile.Class parent) {
		Tuple<Decl.Variable> parameters = expr.getParameters();
		if (i >= parameters.size()) {
			// base case
			JavaFile.Term condition = visitExpression(expr.getOperand(), parent);
			JavaFile.Block trueBranch = new JavaFile.Block();
			if (expr instanceof Expr.UniversalQuantifier) {
				condition = new JavaFile.Operator(JavaFile.Operator.Kind.NOT, condition);
				trueBranch.getTerms().add(new JavaFile.Return(new JavaFile.Constant(false)));
			} else {
				trueBranch.getTerms().add(new JavaFile.Return(new JavaFile.Constant(true)));
			}
			return new JavaFile.If(condition, trueBranch, null);
		} else {
			Decl.Variable parameter = parameters.get(i);
			JavaFile.Type type = JAVA_MATH_BIGINTEGER;
			Expr.ArrayRange range = (Expr.ArrayRange) parameter.getInitialiser();
			JavaFile.Term start = visitExpression(range.getFirstOperand(), parent);
			JavaFile.Term end = visitExpression(range.getSecondOperand(), parent);
			JavaFile.VariableDeclaration decl = new JavaFile.VariableDeclaration(type, parameter.getName().get(),
					start);
			JavaFile.VariableAccess var = new JavaFile.VariableAccess(parameter.getName().get());
			JavaFile.Term condition = new JavaFile.Operator(JavaFile.Operator.Kind.LT,
					new JavaFile.Invoke(var, "compareTo", end), new JavaFile.Constant(0));
			JavaFile.Term increment = new JavaFile.Assignment(var, new JavaFile.Invoke(var, "add",
					new JavaFile.FieldAccess(new JavaFile.VariableAccess("BigInteger"), "ONE")));
			JavaFile.Block body = new JavaFile.Block();
			body.getTerms().add(translateQuantifierBody(expr, i + 1, parent));
			return new JavaFile.For(decl, condition, increment, body);
		}
	}

	/**
	 * Create a simple visitor for extracting all variable access expressions from a
	 * given expression (or statement).
	 */
	private static final AbstractConsumer<HashSet<Decl.Variable>> usedVariableExtractor = new AbstractConsumer<HashSet<Decl.Variable>>() {
		@Override
		public void visitVariableAccess(WhileyFile.Expr.VariableAccess expr, HashSet<Decl.Variable> used) {
			used.add(expr.getVariableDeclaration());
		}

		@Override
		public void visitUniversalQuantifier(WhileyFile.Expr.UniversalQuantifier expr, HashSet<Decl.Variable> used) {
			visitVariables(expr.getParameters(), used);
			visitExpression(expr.getOperand(), used);
			removeAllDeclared(expr.getParameters(), used);
		}

		@Override
		public void visitExistentialQuantifier(WhileyFile.Expr.ExistentialQuantifier expr,
				HashSet<Decl.Variable> used) {
			visitVariables(expr.getParameters(), used);
			visitExpression(expr.getOperand(), used);
			removeAllDeclared(expr.getParameters(), used);
		}

		@Override
		public void visitType(WhileyFile.Type type, HashSet<Decl.Variable> used) {
			// No need to visit types
		}

		private void removeAllDeclared(Tuple<Decl.Variable> parameters, HashSet<Decl.Variable> used) {
			for (int i = 0; i != parameters.size(); ++i) {
				used.remove(parameters.get(i));
			}
		}
	};

	private JavaFile.Term translateRecordSchema(Type type) {
		if (type instanceof Type.Nominal) {
			try {
				Type.Nominal nom = (Type.Nominal) type;
				Decl.Type decl = typeSystem.resolveExactly(nom.getName(), Decl.Type.class);
				return translateRecordSchema(decl.getType());
			} catch (NameResolver.ResolutionError e) {
				throw new RuntimeException(e);
			}
		} else {
			// Must be a record.
			Type.Record rec = (Type.Record) type;
			Tuple<Decl.Variable> fields = rec.getFields();
			StringBuilder builder = new StringBuilder();
			for (int i = 0; i != fields.size(); ++i) {
				if (i != 0) {
					builder.append(",");
				}
				builder.append(fields.get(i).getName().toString());
			}
			return new JavaFile.Constant(builder.toString());
		}
	}

	private JavaFile.Term translateOperator(JavaFile.Operator.Kind kind, Tuple<Expr> operands) {
		JavaFile.Term[] jOperands = new JavaFile.Term[operands.size()];
		for (int i = 0; i != operands.size(); ++i) {
			// FIXME: potentially require coercions here
			jOperands[i] = visitExpression(operands.get(i), null);
		}
		return new JavaFile.Operator(kind, jOperands);
	}

	private JavaFile.Term translateOperator(JavaFile.Operator.Kind kind, Expr... operands) {
		JavaFile.Term[] jOperands = new JavaFile.Term[operands.length];
		for (int i = 0; i != operands.length; ++i) {
			// FIXME: potentially require coercions here
			jOperands[i] = visitExpression(operands[i], null);
		}
		return new JavaFile.Operator(kind, jOperands);
	}

	/**
	 * Translate an integer constant which will be assigned to a fixed-sized Java
	 * variable. Such constants can be safely expressed as integer literals.
	 *
	 * @param constant
	 * @return
	 */
	private static JavaFile.Term translateFixedIntegerConstant(BigInteger constant) {
		long lv = constant.longValue();
		Object value;
		if (lv >= Integer.MIN_VALUE && lv < Integer.MAX_VALUE) {
			value = (int) lv;
		} else {
			value = lv;
		}
		return new JavaFile.Constant(value);
	}

	/**
	 * Translate an integer constant which will be assigned to a dynamically-sized
	 * Java variable. Such constants cannot be expressed as integer literals and
	 * must be converted into instances of <code>BigInteger</code>.
	 *
	 * @param constant
	 * @return
	 */
	private static JavaFile.Term translateUnboundIntegerConstant(BigInteger constant) {
		long lv = constant.longValue();
		// FIXME: bug here for constants which cannot fit inside a long
		return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, new JavaFile.Constant(lv));
	}

	private static JavaFile.Term translateUtf8Constant(byte[] bytes) {
		ArrayList<JavaFile.Term> initialisers = new ArrayList<>();
		for (int i = 0; i != bytes.length; ++i) {
			BigInteger constant = BigInteger.valueOf(bytes[i]);
			initialisers.add(translateUnboundIntegerConstant(constant));
		}
		return new JavaFile.NewArray(new JavaFile.Array(JAVA_MATH_BIGINTEGER), null, initialisers);
	}

	private static List<JavaFile.Modifier> translateModifiers(Tuple<Modifier> modifiers) {
		ArrayList<JavaFile.Modifier> javaModifiers = new ArrayList<>();
		for (Modifier m : modifiers) {
			if (m instanceof Modifier.Public) {
				javaModifiers.add(JavaFile.Modifier.PUBLIC);
			} else if (m instanceof Modifier.Private) {
				javaModifiers.add(JavaFile.Modifier.PRIVATE);
			}
		}
		return javaModifiers;
	}

	/**
	 * Convert a sequence of zero or more types which represent the return values of
	 * a function or method. In the case of exactly one type, then it just returns
	 * the conversion of that type. Otherwise, it returns the type of an object
	 * array which will hold the necessary arguments.
	 *
	 * @param types
	 * @return
	 * @throws ResolutionError
	 */
	private JavaFile.Type translateReturnTypes(Tuple<Type> types) {
		if (types.size() == 0) {
			return JavaFile.VOID;
		} else if (types.size() == 1) {
			return visitType(types.get(0), null);
		} else {
			throw new RuntimeException("Got here");
		}
	}

	private JavaFile.Term registerCoercion(Type target, Expr operand, JavaFile.Class parent) {
		Type source = operand.getType();
		JavaFile.Term jTerm = visitExpression(operand, parent);
		//
		Pair<Type, Type> coercion = new Pair<>(target, source);
		// First, determine whether an identical coercion already exists.
		for (int i = 0; i != coercions.size(); ++i) {
			if (coercions.get(i).equals(coercion)) {
				String name = "coercion" + i;
				return new JavaFile.Invoke(null, name, jTerm);
			}
		}
		// At this point, a coercion for this particular pair of types does not yet
		// exist. Therefore, we register a new one.
		String name = "coercion" + coercions.size();
		coercions.add(coercion);
		// Done
		return new JavaFile.Invoke(null, name, jTerm);
	}

	/**
	 * Translate all the coercions registered for this Java file.
	 *
	 * @param coercions
	 * @param parent
	 */
	private void translateCoercions(List<Pair<Type, Type>> coercions, JavaFile.Class parent) {
		int size = 0;
		while (size < coercions.size()) {
			int start = size;
			size = coercions.size();
			for (int i = start; i != size; ++i) {
				Pair<Type, Type> coercion = coercions.get(i);
				parent.getDeclarations().add(translateCoercion(i, coercion.first(), coercion.second()));
			}
		}
	}

	private JavaFile.Method translateCoercion(int id, Type target, Type source) {
		String name = "coercion" + id;
		JavaFile.Type jTarget = visitType(target, null);
		JavaFile.Type jSource = visitType(source, null);
		// Construct coercion method
		JavaFile.Method method = new JavaFile.Method(name, jTarget);
		method.getParameters().add(new JavaFile.VariableDeclaration(jSource, "var"));
		method.getModifiers().add(JavaFile.Modifier.PRIVATE);
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		JavaFile.Block body = new JavaFile.Block();
		method.setBody(body);
		//
		body.getTerms().add(new JavaFile.Return(translateCoercionBody(target, source)));
		return method;
	}

	private JavaFile.Term translateCoercionBody(Type target, Type source) {
		// First, handle the non-symmetric cases. That is coercions between types of
		// different kinds (e.g. record -> union)
		if (target instanceof Type.Nominal) {
			return translateNominalUnknownCoercion((Type.Nominal) target, source);
		} else if (source instanceof Type.Nominal) {
			return translateUnknownNominalCoercion(target, (Type.Nominal) source);
		}
		// Second, handle symmetric cases. That is coercions between types of the same
		// kinds (e.g. record -> record).
		throw new IllegalArgumentException("invalid coercion target");
	}

	private JavaFile.Term translateNominalUnknownCoercion(Type.Nominal target, Type source) {
		try {
			Decl.Type decl = typeSystem.resolveExactly(target.getName(), Decl.Type.class);
			return translateCoercionBody(decl.getType(), source);
		} catch (ResolutionError e) {
			throw new RuntimeException(e);
		}
	}

	private JavaFile.Term translateUnknownNominalCoercion(Type target, Type.Nominal source) {
		try {
			Decl.Type decl = typeSystem.resolveExactly(source.getName(), Decl.Type.class);
			return translateCoercionBody(target, decl.getType());
		} catch (ResolutionError e) {
			throw new RuntimeException(e);
		}
	}

	private JavaFile.Term translateRecordRecordCoercion(Type.Record target, Type.Record source) {
		// Translate field initialisers
		JavaFile.VariableAccess var = new JavaFile.VariableAccess("var");
		ArrayList<JavaFile.Term> initialisers = new ArrayList<>();
		Tuple<Decl.Variable> fields = target.getFields();
		for (int i = 0; i != fields.size(); ++i) {
			Decl.Variable field = fields.get(i);
			initialisers.add(new JavaFile.FieldAccess(var, field.getName().get()));
		}
		// Done
		return new JavaFile.New(WHILEY_RECORD, initialisers);
	}

	private List<JavaFile.Field> visitFieldDeclarations(Type.Record type) {
		ArrayList<JavaFile.Field> fields = new ArrayList<>();
		Tuple<Decl.Variable> typeFields = type.getFields();
		for (int i = 0; i != typeFields.size(); ++i) {
			Decl.Variable f = typeFields.get(i);
			JavaFile.Type fieldType = visitType(f.getType(), null);
			fields.add(new JavaFile.Field(fieldType, f.getName().toString()));
		}
		return fields;
	}

	public boolean isDynamicallySized(Type type) {
		try {
			// FIXME: this is basically completely broken.
			if (typeSystem.isRawCoerciveSubtype(Type.Bool, type, null)) {
				return false;
			} else if (typeSystem.isRawCoerciveSubtype(Type.Byte, type, null)) {
				return false;
			} else if (typeSystem.isRawCoerciveSubtype(Type.Null, type, null)) {
				return false;
			} else {
				return true;
			}
		} catch (NameResolver.ResolutionError e) {
			throw new RuntimeException(e);
		}
	}

	private static Type TYPE(String... args) {
		Identifier[] components = new Identifier[args.length];
		for (int i = 0; i != args.length; ++i) {
			components[i] = new Identifier(args[i]);
		}
		return new Type.Nominal(new Name(components));
	}

	private static JavaFile.Array JAVA_LANG_STRING_ARRAY = new JavaFile.Array(new JavaFile.Reference("String"));
	private static JavaFile.Reference JAVA_MATH_BIGINTEGER = new JavaFile.Reference("BigInteger");
	private static JavaFile.Reference JAVA_LANG_OBJECT = new JavaFile.Reference("Object");
	private static final JavaFile.Reference WHILEY_RECORD = new JavaFile.Reference("Wy", "Struct");

	/**
	 * Convert a given term which returns a value of integer type into a Java int.
	 * Essentially, if the returned value is a BigInteger then this will invoke
	 * <code>BigInteger.intValue()</code>. Otherwise, it's a no-operation.
	 *
	 * @param term
	 *            The Java term whose value is being converted to an
	 *            <code>int</code>.
	 * @param type
	 *            The Whiley type associated with the given term.
	 * @return
	 * @throws ResolutionError
	 * @throws ResolveError
	 */
	private JavaFile.Term toInt(JavaFile.Term term, Type type) {
		if (isDynamicallySized(type)) {
			return new JavaFile.Invoke(term, new String[] { "intValue" });
		} else {
			return term;
		}
	}

	/**
	 * Convert a given term which returns a value of Java int or long type into a
	 * Java BigInteger. Essentially, if the returned value is a such a type then
	 * this will invoke <code>BigInteger.valueOf()</code>. Otherwise, it's a
	 * no-operation.
	 *
	 * @param term
	 *            The Java term whose value is being converted to a
	 *            <code>BigInteger</code>.
	 * @param type
	 *            The Whiley type associated with the given term.
	 * @return
	 * @throws ResolutionError
	 * @throws ResolveError
	 */
	private JavaFile.Term toBigInteger(JavaFile.Term term, Type type) {
		return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, term);
	}

	/**
	 * Return true if the type in question can be copied directly. More
	 * specifically, if a bitwise copy of the value is sufficient to fully copy it.
	 * In general, this is true for primitive data types in Java. But, for array
	 * types or general class types, it is not true (since these are references into
	 * the heap). As an exception, class types which are known to be immutable can
	 * be safely considered as copyable.
	 *
	 * @param type
	 * @return
	 * @throws ResolutionError
	 */
	private boolean isCopyable(WhileyFile.Type type) {
		try {
			if (type instanceof WhileyFile.Type.Primitive || type instanceof WhileyFile.Type.Callable) {
				return true;
			} else if (type instanceof WhileyFile.Type.Nominal) {
				Type.Nominal tn = (Type.Nominal) type;
				Decl.Type decl = typeSystem.resolveExactly(tn.getName(), Decl.Type.class);
				return isCopyable(decl.getType());
			} else {
				// FIXME: could do better here, e.g. for immutable reference types
				return false;
			}
		} catch (NameResolver.ResolutionError e) {
			throw new RuntimeException(e);
		}
	}

	private static JavaFile.Operator.Kind visit2JavaOperator(int k) {
		switch (k) {
		case EXPR_integernegation:
			return JavaFile.Operator.Kind.NEG;
		case EXPR_logicalnot:
			return JavaFile.Operator.Kind.NOT;
		// Binary
		case EXPR_integeraddition:
			return JavaFile.Operator.Kind.ADD;
		case EXPR_integersubtraction:
			return JavaFile.Operator.Kind.SUB;
		case EXPR_integermultiplication:
			return JavaFile.Operator.Kind.MUL;
		case EXPR_integerdivision:
			return JavaFile.Operator.Kind.DIV;
		case EXPR_integerremainder:
			return JavaFile.Operator.Kind.REM;
		case EXPR_equal:
			return JavaFile.Operator.Kind.EQ;
		case EXPR_notequal:
			return JavaFile.Operator.Kind.NEQ;
		case EXPR_integerlessthan:
			return JavaFile.Operator.Kind.LT;
		case EXPR_integerlessequal:
			return JavaFile.Operator.Kind.LTEQ;
		case EXPR_integergreaterthan:
			return JavaFile.Operator.Kind.GT;
		case EXPR_integergreaterequal:
			return JavaFile.Operator.Kind.GTEQ;
		case EXPR_logicaland:
			return JavaFile.Operator.Kind.AND;
		case EXPR_logicalor:
			return JavaFile.Operator.Kind.OR;
		case EXPR_bitwiseor:
			return JavaFile.Operator.Kind.BITWISEOR;
		case EXPR_bitwisexor:
			return JavaFile.Operator.Kind.BITWISEXOR;
		case EXPR_bitwiseand:
			return JavaFile.Operator.Kind.BITWISEAND;
		case EXPR_bitwisenot:
			return JavaFile.Operator.Kind.BITWISEINVERT;
		case EXPR_bitwiseshl:
			return JavaFile.Operator.Kind.LEFTSHIFT;
		case EXPR_bitwiseshr:
			return JavaFile.Operator.Kind.RIGHTSHIFT;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + k);
		}
	}
}
