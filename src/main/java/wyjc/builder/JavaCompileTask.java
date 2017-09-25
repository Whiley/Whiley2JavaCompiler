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
import wybs.lang.NameResolver.ResolutionError;
import wyc.lang.WhileyFile;
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

public class JavaCompileTask implements Build.Task {
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

	/**
	 * The set of names for all anonymous structs and unions encountered.
	 */
	private HashMap<Type, JavaFile.Class> unnamedTypes;

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
		// Reset the list of unnamed types
		unnamedTypes = new HashMap<>();
		// Create an (empty) output file to contains the generated Java source
		// code.
		JavaFile jf = new JavaFile(target);

		// Add package declaration?

		// Add imports
		jf.getDeclarations().add(new JavaFile.Import("java", "math", "BigInteger"));
		jf.getDeclarations().add(new JavaFile.Import("java", "util", "Arrays"));
		jf.getDeclarations().add(new JavaFile.Import("java", "util", "function", "Function"));

		String className = wf.getEntry().id().last();
		JavaFile.Class jcd = new JavaFile.Class(className);
		jcd.getModifiers().add(JavaFile.Modifier.PUBLIC);
		jcd.getModifiers().add(JavaFile.Modifier.FINAL);
		// Translate all declarations
		for (Decl b : wf.getDeclarations()) {
			try {
				if (b instanceof Decl.StaticVariable) {
					// ?
				} else if (b instanceof Decl.FunctionOrMethod) {
					build((Decl.FunctionOrMethod) b, jcd);
				} else if (b instanceof Decl.Type) {
					build((Decl.Type) b, jcd);
				}
			} catch (ResolutionError e) {
				throw new RuntimeException(e);
			}
		}
		// Add all generated unnamed types
		for (Map.Entry<Type, JavaFile.Class> e : unnamedTypes.entrySet()) {
			jcd.getDeclarations().add(e.getValue());
		}
		//
		jf.getDeclarations().add(jcd);
		return jf;
	}

	private void build(Decl.Type decl, JavaFile.Class parent) throws ResolutionError {
		Type type = decl.getType();

		// FIXME: deal with Nominal types ?

		if (type instanceof Type.Record) {
			//
			List<JavaFile.Field> fields = translateFieldDeclarations((Type.Record) type);
			JavaFile.Class struct = JavaCodeGenerator.generateStruct(decl.getName().toString(), fields);
			struct.getModifiers().addAll(translateModifiers(decl.getModifiers()));
			parent.getDeclarations().add(struct);
		} else if (type instanceof Type.Union) {
			Type.Union ut = (Type.Union) type;
			JavaFile.Type[] bounds = translateTypes(ut.toArray(Type.class));
			JavaFile.Class union = JavaCodeGenerator.generateUnion(decl.getName().toString(), bounds);
			union.getModifiers().addAll(translateModifiers(decl.getModifiers()));
			parent.getDeclarations().add(union);
		}
	}

	private void build(Decl.FunctionOrMethod decl, JavaFile.Class parent) throws ResolutionError {
		Type.Callable sig = decl.getType();
		JavaFile.Type returnType = translateReturnTypes(sig.getReturns());
		//
		JavaFile.Method method = new JavaFile.Method(decl.getName().toString(), returnType);
		method.getModifiers().addAll(translateModifiers(decl.getModifiers()));
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		//
		List<JavaFile.VariableDeclaration> parameters = translateVariableDeclarations(decl.getParameters());
		method.getParameters().addAll(parameters);
		//
		// FIXME: should provide support for checking preconditions /
		// postconditions here. These can be implemented as runtime checks.
		//
		method.setBody(translateBlock(decl.getBody()));
		parent.getDeclarations().add(method);
	}

	private List<JavaFile.VariableDeclaration> translateVariableDeclarations(Tuple<Decl.Variable> decls)
			throws ResolutionError {
		List<JavaFile.VariableDeclaration> result = new ArrayList<>();
		for (int i = 0; i != decls.size(); ++i) {
			Decl.Variable pd = decls.get(i);
			JavaFile.Type pt = translateType(pd.getType());
			result.add(new JavaFile.VariableDeclaration(pt, pd.getName().toString(), null));
		}
		return result;
	}

	private JavaFile.Term translateVariableDeclaration(Decl.Variable stmt) throws ResolutionError {
		JavaFile.Type type = translateType(stmt.getType());
		JavaFile.Term initialiser = null;
		if (stmt.hasInitialiser()) {
			initialiser = translateExpression(stmt.getInitialiser());
		}
		// FIXME: potential for mismatch between Whiley identifiers and Java
		// identifiers.
		return new JavaFile.VariableDeclaration(type, stmt.getName().get(), initialiser);
	}

	private JavaFile.Block translateBlock(Stmt.Block block) {
		JavaFile.Block jblock = new JavaFile.Block();
		for (int i = 0; i != block.size(); ++i) {
			JavaFile.Term jterm = translateStatement(block.get(i));
			jblock.getTerms().add(jterm);
		}
		return jblock;
	}

	private JavaFile.Term translateStatement(Stmt c) {
		try {
			switch (c.getOpcode()) {
			case STMT_assert:
				return translateAssert((Stmt.Assert) c);
			case STMT_assume:
				return translateAssume((Stmt.Assume) c);
			case STMT_assign:
				return translateAssign((Stmt.Assign) c);
			case STMT_break:
				return translateBreak((Stmt.Break) c);
			case STMT_continue:
				return translateContinue((Stmt.Continue) c);
			// case STMT_debug:
			// return translateDebug((Stmt.Debug) c);
			case STMT_dowhile:
				return translateDoWhile((Stmt.DoWhile) c);
			// case STMT_fail:
			// return translateFail((Stmt.Fail) c);
			case STMT_if:
			case STMT_ifelse:
				return translateIf((Stmt.IfElse) c);
			case EXPR_indirectinvoke:
				return translateIndirectInvoke((Expr.IndirectInvoke) c);
			case EXPR_invoke:
				return translateInvoke((Expr.Invoke) c);
			// case STMT_namedblock:
			// return translateBlock((Stmt.NamedBlock) c);
			case STMT_while:
				return translateWhile((Stmt.While) c);
			case STMT_return:
				return translateReturn((Stmt.Return) c);
			case STMT_switch:
				return translateSwitch((Stmt.Switch) c);
			case DECL_variable:
			case DECL_variableinitialiser:
				return translateVariableDeclaration((Decl.Variable) c);
			default:
				throw new IllegalArgumentException("unknown bytecode encountered");
			}
		} catch (ResolutionError e) {
			// should be deadcode
			throw new IllegalArgumentException(e);
		}
	}

	private JavaFile.Term translateAssert(Stmt.Assert stmt) {
		JavaFile.Term operand = translateExpression(stmt.getCondition());
		System.out.println("OPERAND = " + operand);
		return new JavaFile.Assert(operand);
	}

	private JavaFile.Term translateAssign(Stmt.Assign stmt) {
		List<JavaFile.Term> lhs = translateExpressions((Tuple) stmt.getLeftHandSide());
		List<JavaFile.Term> rhs = translateExpressions(stmt.getRightHandSide());
		if (lhs.size() > 1) {
			throw new IllegalArgumentException("Need support for multiple assignments");
		} else {
			return new JavaFile.Assignment(lhs.get(0), rhs.get(0));
		}
	}

	private JavaFile.Term translateAssume(Stmt.Assume stmt) {
		JavaFile.Term operand = translateExpression(stmt.getCondition());
		return new JavaFile.Assert(operand);
	}

	private JavaFile.Term translateBreak(Stmt.Break stmt) {
		return new JavaFile.Break();
	}

	private JavaFile.Term translateContinue(Stmt.Continue stmt) {
		return new JavaFile.Continue();
	}

	private JavaFile.Term translateDoWhile(Stmt.DoWhile stmt) {
		JavaFile.Term condition = translateExpression(stmt.getCondition());
		JavaFile.Block body = translateBlock(stmt.getBody());
		return new JavaFile.DoWhile(body, condition);
	}

	private JavaFile.Term translateIf(Stmt.IfElse stmt) {
		JavaFile.Term operand = translateExpression(stmt.getCondition());
		JavaFile.Block trueBranch = translateBlock(stmt.getTrueBranch());
		JavaFile.Block falseBranch = null;
		if (stmt.hasFalseBranch()) {
			falseBranch = translateBlock(stmt.getFalseBranch());
		}
		return new JavaFile.If(operand, trueBranch, falseBranch);
	}

	private JavaFile.Term translateReturn(Stmt.Return stmt) {
		JavaFile.Term initialiser = null;
		Tuple<Expr> returns = stmt.getReturns();
		if (returns.size() > 0) {
			List<JavaFile.Term> tmp = translateExpressions(returns);
			if (tmp.size() > 1) {
				throw new RuntimeException("need to implement multiple returns");
			}
			initialiser = tmp.get(0);
		}
		return new JavaFile.Return(initialiser);
	}

	private JavaFile.Term translateSwitch(Stmt.Switch stmt) throws ResolutionError {
		Tuple<Stmt.Case> cases = stmt.getCases();
		JavaFile.Term[][] groups = new JavaFile.Term[cases.size()][];
		// Translate all case conditions and see whether can use a switch or not.
		boolean canSwitch = true;
		for(int i=0;i!=groups.length;++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			JavaFile.Term[] terms = new JavaFile.Term[conditions.size()];
			for(int j=0;j!=terms.length;++j) {
				terms[j] = translateExpression(conditions.get(j));
				canSwitch &= (terms[j] instanceof JavaFile.Constant);
			}
			groups[i] = terms;
		}
		//
		if(canSwitch) {
			// Yes, we can switch
			return translateSwitchAsSwitch(stmt,groups);
		} else {
			return translateSwitchAsIfChain(stmt,groups);
		}
		//

	}

	private JavaFile.Term translateSwitchAsSwitch(Stmt.Switch stmt, JavaFile.Term[][] groups) {
		JavaFile.Term condition = translateExpression(stmt.getCondition());
		Tuple<Stmt.Case> cases = stmt.getCases();
		ArrayList<JavaFile.Case> jCases = new ArrayList<>();
		for (int i = 0; i != cases.size(); ++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			for (int j = 0; j != conditions.size(); ++j) {
				JavaFile.Constant label = (JavaFile.Constant) groups[i][j];
				JavaFile.Block body = translateBlock(c.getBlock());
				jCases.add(new JavaFile.Case(label, body));
			}
		}
		return new JavaFile.Switch(condition, jCases);
	}

	private JavaFile.Term translateSwitchAsIfChain(Stmt.Switch stmt, JavaFile.Term[][] groups) throws ResolutionError {
		// FIXME: this obviously doesn't work --- need to figure out a proper temporary
		// variable name.
		String tmpVar = "tmp";
		ArrayList<JavaFile.Term> stmts = new ArrayList<>();
		Tuple<Stmt.Case> cases = stmt.getCases();
		JavaFile.Term condition = translateExpression(stmt.getCondition());
		JavaFile.Type type = translateType(stmt.getCondition().getType());
		stmts.add(new JavaFile.VariableDeclaration(type, tmpVar, condition));
		//
		ArrayList<JavaFile.IfElse.Case> ifconditions = new ArrayList<>();
		for (int i = 0; i != cases.size(); ++i) {
			Stmt.Case c = cases.get(i);
			Tuple<Expr> conditions = c.getConditions();
			JavaFile.Term cas = null;
			for (int j = 0; j != conditions.size(); ++j) {
				// Translate the value itself
				JavaFile.Term t = translateExpression(conditions.get(j));
				// Convert it into an equality
				t = translateOperator(WhileyFile.EXPR_equal, stmt.getCondition().getType(),
						new JavaFile.VariableAccess(tmpVar), t);
				cas = (j == 0) ? t : new JavaFile.Operator(JavaFile.Operator.Kind.AND, cas, t);
			}
			JavaFile.Block body = translateBlock(c.getBlock());
			ifconditions.add(new JavaFile.IfElse.Case(cas, body));
		}
		stmts.add(new JavaFile.IfElse(ifconditions));
		return new JavaFile.Block(stmts);
	}

	private JavaFile.Term translateWhile(Stmt.While stmt) {
		JavaFile.Term condition = translateExpression(stmt.getCondition());
		JavaFile.Block body = translateBlock(stmt.getBody());
		return new JavaFile.While(condition, body);
	}

	// private JavaFile.Term[] translateExpressions(Tuple<Expr> expr) {
	// JavaFile.Term[] terms = new JavaFile.Term[expr.size()];
	// for (int i = 0; i != terms.length; ++i) {
	// terms[i] = translateExpression(expr.get(i));
	// }
	// return terms;
	// }

	private List<JavaFile.Term> translateExpressions(Tuple<Expr> exprs) {
		List<JavaFile.Term> arguments = new ArrayList<>();
		for (int i = 0; i != exprs.size(); ++i) {
			arguments.add(translateExpression(exprs.get(i)));
		}
		return arguments;
	}

	// @SuppressWarnings("unchecked")
	private JavaFile.Term translateExpression(Expr expr) {
		try {
			switch (expr.getOpcode()) {
			case EXPR_logicalnot:
			case EXPR_integernegation:
			case EXPR_dereference:
			case EXPR_new:
			case EXPR_bitwisenot:
			case EXPR_is:
				return translateUnaryOperator((Expr.UnaryOperator) expr);
			case EXPR_bitwiseshl:
			case EXPR_bitwiseshr:
			case EXPR_integeraddition:
			case EXPR_integersubtraction:
			case EXPR_integermultiplication:
			case EXPR_integerdivision:
			case EXPR_integerremainder:
			case EXPR_equal:
			case EXPR_notequal:
			case EXPR_integerlessthan:
			case EXPR_integerlessequal:
			case EXPR_integergreaterthan:
			case EXPR_integergreaterequal:
				return translateBinaryOperator((Expr.BinaryOperator) expr);
			case EXPR_logicaland:
			case EXPR_logicalor:
			case EXPR_bitwiseor:
			case EXPR_bitwisexor:
			case EXPR_bitwiseand:
				return translateNaryOperator((Expr.NaryOperator) expr);
			case EXPR_arraylength:
				return translateArrayLength((Expr.ArrayLength) expr);
			case EXPR_arrayaccess:
				return translateArrayAccess((Expr.ArrayAccess) expr);
			case EXPR_arrayinitialiser:
				return translateArrayInitialiser((Expr.ArrayInitialiser) expr);
			case EXPR_arraygenerator:
				return translateArrayGenerator((Expr.ArrayGenerator) expr);
			case EXPR_cast:
				return translateConvert((Expr.Cast) expr);
			case EXPR_constant:
				return translateConst((Expr.Constant) expr);
			case EXPR_recordaccess:
				return translateFieldLoad((Expr.RecordAccess) expr);
			case EXPR_indirectinvoke:
				return translateIndirectInvoke((Expr.IndirectInvoke) expr);
			case EXPR_invoke:
				return translateInvoke((Expr.Invoke) expr);
			case EXPR_lambdaaccess:
				return translateLambdaAccess((Expr.LambdaAccess) expr);
			case DECL_lambda:
				return translateLambda((Decl.Lambda) expr);
			case EXPR_recordinitialiser:
				return translateRecordInitialiser((Expr.RecordInitialiser) expr);
			case EXPR_logicaluniversal:
			case EXPR_logicalexistential:
				return translateQuantifier((Expr.Quantifier) expr);
			case EXPR_variablemove:
			case EXPR_variablecopy:
				return translateVariableAccess((Expr.VariableAccess) expr);
			default:
				throw new IllegalArgumentException("unknown expression encountered: " + expr);
			}
		} catch (ResolutionError e) {
			throw new RuntimeException("internal failure: " + e.getMessage(), e);
		}
	}

	private JavaFile.Term translateUnaryOperator(Expr.UnaryOperator expr) throws ResolutionError {
		return translateOperator(expr.getOpcode(), expr.getOperand());
	}

	private JavaFile.Term translateBinaryOperator(Expr.BinaryOperator expr) throws ResolutionError {
		return translateOperator(expr.getOpcode(), expr.getFirstOperand(), expr.getSecondOperand());
	}

	private JavaFile.Term translateNaryOperator(Expr.NaryOperator expr) throws ResolutionError {
		return translateOperator(expr.getOpcode(), expr.getOperands().toArray(Expr.class));
	}

	private JavaFile.Term translateOperator(int kind, Expr... operands) throws ResolutionError {
		// First, translate all children of the operator.
		JavaFile.Term[] children = new JavaFile.Term[operands.length];
		for (int i = 0; i != operands.length; ++i) {
			children[i] = translateExpression(operands[i]);
		}
		Type argumentType = operands[0].getType();
		return translateOperator(kind,argumentType,children);
	}
	private JavaFile.Term translateOperator(int kind, Type argumentType, JavaFile.Term... children) throws ResolutionError {
		//

		if (isDynamicallySized(argumentType)) {
			// In this case, we have dynamically-sized arguments (e.g.
			// BigInteger). In such case, we must exploit the various methods on
			// Object or BigInteger for evaluating the operation.

			// FIXME: this coercion should be deprecated with proper support for
			// fixed-size types in the Whiley Compiler.
			for (int i = 0; i != children.length; ++i) {
				children[i] = toBigInteger(children[i], argumentType);
			}
			//
			switch (kind) {
			case EXPR_integernegation:
			case EXPR_integeraddition:
			case EXPR_integersubtraction:
			case EXPR_integermultiplication:
			case EXPR_integerdivision:
			case EXPR_integerremainder:
				return translateUnboundArithmeticOperator(kind, children);
			case EXPR_equal:
			case EXPR_notequal:
			case EXPR_integerlessthan:
			case EXPR_integerlessequal:
			case EXPR_integergreaterthan:
			case EXPR_integergreaterequal:
				return translateUnboundComparator(kind, children);
			default:
				throw new IllegalArgumentException("Unknown expression encountered");
			}
		} else {
			// In this case, we have a fixed-sized arguments. Therefore, we can
			// employ Java's underlying arithmetic operators, comparators, etc.
			JavaFile.Operator.Kind k = translate2JavaOperator(kind);
			// Apply any necessary coercions
			switch (kind) {
			case EXPR_bitwiseshl:
			case EXPR_bitwiseshr: {
				// FIXME: in principle, this should be unnecesssary as the
				// WhileyCompiler should take care of this.
				children[1] = toInt(children[1], argumentType);
			}
			case EXPR_bitwisexor:
			case EXPR_bitwiseor:
			case EXPR_bitwiseand: {
				JavaFile.Term result = new JavaFile.Operator(k, children);
				return new JavaFile.Cast(JavaFile.BYTE, result);
			}

			}
			return new JavaFile.Operator(k, children);
		}
	}

	/**
	 * Translate an unbound arithmetic operator. For example, the following Whiley
	 * code:
	 *
	 * <pre>
	 * int x = 1
	 * int y = x + 2
	 * </pre>
	 *
	 * would translate into the following Java code:
	 *
	 * <pre>
	 * BigInteger x = BigInteger.valueOf(1);
	 * BigInteger y = x.add(BigInteger.valueOf(1));
	 * </pre>
	 *
	 * @param opcode
	 * @param operands
	 * @return
	 */
	private static JavaFile.Term translateUnboundArithmeticOperator(int opcode, JavaFile.Term... operands) {
		String methodName;
		switch (opcode) {
		case EXPR_integernegation:
			methodName = "negate";
			break;
		case EXPR_integeraddition:
			methodName = "add";
			break;
		case EXPR_integersubtraction:
			methodName = "subtract";
			break;
		case EXPR_integermultiplication:
			methodName = "multiply";
			break;
		case EXPR_integerdivision:
			methodName = "divide";
			break;
		case EXPR_integerremainder:
			methodName = "remainder";
			break;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + opcode);
		}
		// Construct the method invocation
		JavaFile.Term receiver = operands[0];
		if (operands.length == 1) {
			return new JavaFile.Invoke(receiver, new String[] { methodName });
		} else {
			JavaFile.Term operand = operands[1];
			return new JavaFile.Invoke(receiver, new String[] { methodName }, operand);
		}
	}

	private static JavaFile.Term translateUnboundComparator(int opcode, JavaFile.Term... operands) {
		JavaFile.Operator.Kind kind;
		JavaFile.Term receiver = operands[0];
		JavaFile.Term operand = operands[1];
		//
		switch (opcode) {
		case EXPR_notequal: {
			JavaFile.Term eq = new JavaFile.Invoke(receiver, new String[] { "equals" }, operand);
			return new JavaFile.Operator(JavaFile.Operator.Kind.NOT, eq);
		}
		case EXPR_equal: {
			return new JavaFile.Invoke(receiver, new String[] { "equals" }, operand);
		}
		case EXPR_integerlessthan:
			kind = JavaFile.Operator.Kind.LT;
			break;
		case EXPR_integerlessequal:
			kind = JavaFile.Operator.Kind.LTEQ;
			break;
		case EXPR_integergreaterthan:
			kind = JavaFile.Operator.Kind.GT;
			break;
		case EXPR_integergreaterequal:
			kind = JavaFile.Operator.Kind.GTEQ;
			break;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + opcode);
		}
		// Construct the method invocation
		JavaFile.Term cmp = new JavaFile.Invoke(receiver, new String[] { "compareTo" }, operand);
		return new JavaFile.Operator(kind, cmp, new JavaFile.Constant(0));
	}

	private JavaFile.Term translateArrayAccess(Expr.ArrayAccess expr) throws ResolutionError {
		JavaFile.Term src = translateExpression(expr.getFirstOperand());
		JavaFile.Term index = translateExpression(expr.getSecondOperand());
		return new JavaFile.ArrayAccess(src, toInt(index, Type.Int));
	}

	private JavaFile.Term translateArrayGenerator(Expr.ArrayGenerator expr) {
		throw new IllegalArgumentException("IMPLEMENT ME");
	}

	private JavaFile.Term translateArrayInitialiser(Expr.ArrayInitialiser expr) throws ResolutionError {
		Tuple<Expr> operands = expr.getOperands();
		List<JavaFile.Term> children = new ArrayList<>();
		for (int i = 0; i != operands.size(); ++i) {
			children.add(translateExpression(operands.get(i)));
		}
		JavaFile.Array type = (JavaFile.Array) translateType(expr.getType());
		return new JavaFile.NewArray(type, null, children);
	}

	private JavaFile.Term translateArrayLength(Expr.ArrayLength expr) throws ResolutionError {
		JavaFile.Term src = translateExpression(expr.getOperand());
		// FIXME: converting the array length to a big integer is a temporary
		// fix. It works around the fact that the Whiley compiler types the
		// return of an array length expression as just "int", when in fact it
		// should be: "usize".
		return toBigInteger(new JavaFile.FieldAccess(src, "length"), TYPE_I32);
	}

	private JavaFile.Term translateConvert(Expr.Cast expr) {
		// out.print("(");
		// writeType(expr.getType());
		// out.print(") ");
		// writeExpression(expr.getOperand(0));
		return null;
	}

	private JavaFile.Term translateConst(Expr.Constant expr) throws ResolutionError {
		return translateConstant(expr.getValue());
	}

	private JavaFile.Term translateConstant(Value c) throws ResolutionError {
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
			BigInteger bi = bc.get();
			Type type = inferIntegerType(bi);
			//
			if (isDynamicallySized(type)) {
				return translateUnboundIntegerConstant(bi);
			} else {
				return translateFixedIntegerConstant(bi);
			}
		} else {
			return translateUtf8Constant(((Value.UTF8) c).get());
		}
		return new JavaFile.Constant(value);
	}

	private static long i8_MIN = -128;
	private static long i8_MAX = 127;
	private static long u8_MAX = 255;
	private static long i16_MIN = -32768;
	private static long i16_MAX = 32767;
	private static long u16_MAX = 65535;
	private static long i32_MIN = Integer.MIN_VALUE;
	private static long i32_MAX = Integer.MAX_VALUE;;
	private static long u32_MAX = 2147483648L;

	private Type inferIntegerType(BigInteger i) {
		try {
			long v = i.longValueExact();
			// FIXME: the exact best way to handle this is unclear to me.
			// if (v >= 0 && v <= u8_MAX) {
			// return TYPE_U8;
			// } else if (v >= 0 && v <= u16_MAX) {
			// return TYPE_U16;
			// } else if (v >= 0 && v <= u32_MAX) {
			// return TYPE_U32;
			// } else if (v >= i8_MIN && v <= i8_MAX) {
			// return TYPE_I8;
			// } else if (v >= i16_MIN && v <= i16_MAX) {
			// return TYPE_I16;
			// } else if (v >= i32_MIN && v <= i32_MAX) {
			// return TYPE_I32;
			// }
		} catch (ArithmeticException ex) {
		}
		return Type.Int;
	}

	/**
	 * Translate an integer constant which will be assigned to a fixed-sized Java
	 * variable. Such constants can be safely expressed as integer literals.
	 *
	 * @param constant
	 * @return
	 */
	private JavaFile.Term translateFixedIntegerConstant(BigInteger constant) {
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
	private JavaFile.Term translateUnboundIntegerConstant(BigInteger constant) {
		long lv = constant.longValue();
		// FIXME: bug here for constants which cannot fit inside a long
		return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, new JavaFile.Constant(lv));
	}

	private JavaFile.Term translateUtf8Constant(byte[] bytes) {
		ArrayList<JavaFile.Term> initialisers = new ArrayList<>();
		for (int i = 0; i != bytes.length; ++i) {
			BigInteger constant = BigInteger.valueOf(bytes[i]);
			initialisers.add(translateUnboundIntegerConstant(constant));
		}
		return new JavaFile.NewArray(new JavaFile.Array(JAVA_MATH_BIGINTEGER), null, initialisers);
	}

	private JavaFile.Term translateFieldLoad(Expr.RecordAccess expr) {
		JavaFile.Term source = translateExpression(expr.getOperand());
		return new JavaFile.FieldAccess(source, expr.getField().toString());
	}

	private JavaFile.Term translateIndirectInvoke(Expr.IndirectInvoke expr) {
		JavaFile.Term receiver = translateExpression(expr.getSource());
		List<JavaFile.Term> arguments = translateExpressions(expr.getArguments());
		return new JavaFile.Invoke(receiver, "apply", arguments);
	}

	private JavaFile.Term translateInvoke(Expr.Invoke expr) {
		List<String> path = new ArrayList<>();
		Tuple<Expr> operands = expr.getOperands();
		List<JavaFile.Term> arguments = new ArrayList<>();
		// FIXME: need to do better here
		path.add(expr.getName().toNameID().name());
		for (int i = 0; i != operands.size(); ++i) {
			arguments.add(translateExpression(operands.get(i)));
		}
		return new JavaFile.Invoke(null, path, arguments);
	}

	private JavaFile.Term translateLambdaAccess(Expr.LambdaAccess expr) throws ResolutionError {
		Tuple<Type> types = expr.getSignature().getParameters();
		List<JavaFile.VariableDeclaration> parameters = new ArrayList<>();
		List<JavaFile.Term> arguments = new ArrayList<>();
		for (int i = 0; i != types.size(); ++i) {
			String name = "p" + i;
			JavaFile.Type type = translateType(types.get(i));
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
	private JavaFile.Term translateLambda(Decl.Lambda expr) {
		// out.print("&[");
		// Location<?>[] environment =
		// expr.getOperandGroup(SyntaxTree.ENVIRONMENT);
		// for (int i = 0; i != environment.length; ++i) {
		// Location<VariableDeclaration> var = (Location<VariableDeclaration>)
		// environment[i];
		// if (i != 0) {
		// out.print(", ");
		// }
		// out.print(var.getType());
		// out.print(" ");
		// out.print(var.getBytecode().getName());
		// }
		// out.print("](");
		// Location<?>[] parameters =
		// expr.getOperandGroup(SyntaxTree.PARAMETERS);
		// for (int i = 0; i != parameters.length; ++i) {
		// Location<VariableDeclaration> var = (Location<VariableDeclaration>)
		// parameters[i];
		// if (i != 0) {
		// out.print(", ");
		// }
		// out.print(var.getType());
		// out.print(" ");
		// out.print(var.getBytecode().getName());
		// }
		// out.print(" -> ");
		// writeExpression(expr.getOperand(0));
		// out.print(")");
		return null;
	}

	private JavaFile.Term translateRecordInitialiser(Expr.RecordInitialiser expr) throws ResolutionError {
		Type type = null; // expr.getType(); // declared type (if applicable)
		JavaFile.Reference jType;
		//
		if (type instanceof Type.Nominal) {
			Type.Nominal nt = (Type.Nominal) type;
			// FIXME: this is surely broken
			jType = new JavaFile.Reference(nt.getName().toNameID().name());
		} else if (type instanceof Type.Record) {
			JavaFile.Class unnamedStruct = generateAnonymousStruct((Type.Record) type);
			jType = new JavaFile.Reference(unnamedStruct.getName());
		} else {
			throw new IllegalArgumentException("Not sure what to do here");
		}
		// Construct the appropriate record now
		Tuple<Expr> operands = expr.getOperands();
		ArrayList<JavaFile.Term> parameters = new ArrayList<>();
		for (int i = 0; i != operands.size(); ++i) {
			Expr operand = operands.get(i);
			parameters.add(translateExpression(operand));
		}
		return new JavaFile.New(jType, parameters);
	}

	@SuppressWarnings("unchecked")
	private JavaFile.Term translateQuantifier(Expr.Quantifier c) {
		// FIXME: Translate into a for loop embedded in a Block. Then JavaFileWriter or
		// JvmFileWriter is responsible for dealing with it.
		return new JavaFile.Constant(false);
	}

	private JavaFile.Term translateVariableAccess(Expr.VariableAccess expr) throws ResolutionError {
		Decl.Variable vd = expr.getVariableDeclaration();
		JavaFile.Term t = new JavaFile.VariableAccess(vd.getName().toString());
		JavaFile.Type type = translateType(expr.getType());
		if (expr.getOpcode() == EXPR_variablecopy && !isCopyable(expr.getType())) {
			// Since this type is not copyable, we need to clone it to ensure
			// that ownership is properly preserved.
			t = new JavaFile.Invoke(t, "clone");
		}
		return t;
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
	private JavaFile.Type translateReturnTypes(Tuple<Type> types) throws ResolutionError {
		if (types.size() == 0) {
			return JavaFile.VOID;
		} else if (types.size() == 1) {
			return translateType(types.get(0));
		} else {
			throw new RuntimeException("Got here");
		}
	}

	/**
	 * Convert a Whiley type into its equivalent Java form. For example, the Whiley
	 * type <code>int:32</code> corresponds to the Java type <code>int</code>. Some
	 * conversions are <i>lossy</i>, meaning that we lose information during the
	 * conversion. For example, the Whiley union type <code>int|bool</code> is
	 * reduced to just <code>Object</code> in Java.
	 *
	 * @param type
	 * @throws ResolutionError
	 */
	private JavaFile.Type translateType(Type type) throws ResolutionError {
		JavaFile.Type jtype = typeMap.get(type);
		if (jtype != null) {
			return jtype;
		} else if (type instanceof Type.Array) {
			Type.Array arrT = (Type.Array) type;
			return new JavaFile.Array(translateType(arrT.getElement()));
		} else if (type instanceof Type.Nominal) {
			Type.Nominal tn = (Type.Nominal) type;
			Decl.Type decl = typeSystem.resolveExactly(tn.getName(), Decl.Type.class);
			if (decl.getType() instanceof Type.Record) {
				return new JavaFile.Reference(tn.getName().toString());
			} else {
				return translateType(decl.getType());
			}
		} else if (type instanceof Type.Record) {
			JavaFile.Class struct = generateAnonymousStruct((Type.Record) type);
			return new JavaFile.Reference(struct.getName());
		} else if (type instanceof Type.Callable) {
			Type.Callable tc = (Type.Callable) type;
			// FIXME: this is a hack for now
			return new JavaFile.Reference("Function");
		} else {
			// Default
			return new JavaFile.Reference("Object");
		}
	}

	private JavaFile.Type[] translateTypes(Type... types) throws ResolutionError {
		JavaFile.Type[] rs = new JavaFile.Type[types.length];
		for (int i = 0; i != types.length; ++i) {
			rs[i] = translateType(types[i]);
		}
		return rs;
	}

	private JavaFile.Class generateAnonymousStruct(Type.Record type) throws ResolutionError {
		JavaFile.Class r = unnamedTypes.get(type);
		if (r == null) {
			List<JavaFile.Field> fields = translateFieldDeclarations(type);
			r = JavaCodeGenerator.generateStruct("Struct" + unnamedTypes.size(), fields);
			unnamedTypes.put(type, r);
		}
		return r;
	}

	private List<JavaFile.Field> translateFieldDeclarations(Type.Record type) throws ResolutionError {
		ArrayList<JavaFile.Field> fields = new ArrayList<>();
		Tuple<Decl.Variable> typeFields = type.getFields();
		for (int i = 0; i != typeFields.size(); ++i) {
			Decl.Variable f = typeFields.get(i);
			JavaFile.Type fieldType = translateType(f.getType());
			fields.add(new JavaFile.Field(fieldType, f.getName().toString()));
		}
		return fields;
	}

	public boolean isDynamicallySized(Type type) throws ResolutionError {
		// FIXME: this is basically completely broken.
		if (type == TYPE_I8 || type == TYPE_I16 || type == TYPE_I32 || type == TYPE_I64 || type == TYPE_U8
				|| type == TYPE_U16 || type == TYPE_U32 || type == TYPE_U64) {
			return false;
		} else if (typeSystem.isRawCoerciveSubtype(Type.Int, type, null)) {
			return true;
		} else if (typeSystem.extractReadableArray(type, null) != null) {
			return true;
		} else {
			// FIXME: need to recursively check component types for records.
			return false;
		}
	}

	private static Type TYPE(String... args) {
		Identifier[] components = new Identifier[args.length];
		for (int i = 0; i != args.length; ++i) {
			components[i] = new Identifier(args[i]);
		}
		return new Type.Nominal(new Name(components));
	}

	private static Type TYPE_I8 = TYPE("std", "integer", "i8");
	private static Type TYPE_I16 = TYPE("std", "integer", "i17");
	private static Type TYPE_I32 = TYPE("std", "integer", "i32");
	private static Type TYPE_I64 = TYPE("std", "integer", "i64");
	private static Type TYPE_U8 = TYPE("std", "integer", "u8");
	private static Type TYPE_U16 = TYPE("std", "integer", "u16");
	private static Type TYPE_U32 = TYPE("std", "integer", "u32");
	private static Type TYPE_U64 = TYPE("std", "integer", "u64");

	private static JavaFile.Reference JAVA_MATH_BIGINTEGER = new JavaFile.Reference("BigInteger");

	private static HashMap<Type, JavaFile.Type> typeMap = new HashMap<Type, JavaFile.Type>() {
		{
			// Integer types
			put(TYPE_I8, JavaFile.BYTE);
			put(TYPE_I16, JavaFile.SHORT);
			put(TYPE_I32, JavaFile.INT);
			put(TYPE_I64, JavaFile.LONG);
			put(TYPE_U8, JavaFile.BYTE);
			put(TYPE_U16, JavaFile.INT);
			put(TYPE_U32, JavaFile.LONG);
			// Others
			put(Type.Bool, JavaFile.BOOLEAN);
			put(Type.Byte, JavaFile.BYTE);
			put(Type.Int, JAVA_MATH_BIGINTEGER);
			put(Type.Any, new JavaFile.Reference("Object"));
		}
	};

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
	private JavaFile.Term toInt(JavaFile.Term term, Type type) throws ResolutionError {
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
	private JavaFile.Term toBigInteger(JavaFile.Term term, Type type) throws ResolutionError {
		if (isDynamicallySized(type)) {
			return term;
		} else {
			return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, term);
		}
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
	private boolean isCopyable(WhileyFile.Type type) throws ResolutionError {
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
	}

	private static JavaFile.Operator.Kind translate2JavaOperator(int k) {
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
