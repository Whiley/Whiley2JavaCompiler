// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.builder;

import static wyil.lang.SyntaxTree.CONDITION;
import static wyil.lang.SyntaxTree.LEFTHANDSIDE;
import static wyil.lang.SyntaxTree.RIGHTHANDSIDE;

import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

import wybs.lang.Build;
import wybs.lang.NameID;
import wybs.util.ResolveError;
import wybs.lang.Build.Graph;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Path;
import wyfs.lang.Path.Entry;
import wyfs.lang.Path.Root;
import wyfs.util.Trie;
import wyil.lang.*;
import wyil.lang.Bytecode.Block;
import wyil.lang.Bytecode.VariableDeclaration;
import wyil.lang.SyntaxTree.Location;
import wyil.util.TypeSystem;
import wyjc.core.JavaFile;
import wyjc.util.JavaCodeGenerator;

public class JavaCompileTask implements Build.Task {
	/**
	 * The master project for identifying all resources available to the
	 * builder. This includes all modules declared in the project being verified
	 * and/or defined in external resources (e.g. jar files).
	 */
	protected final Build.Project project;

	/**
	 * The type system is useful for managing nominal types and converting them
	 * into their underlying types.
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
			Path.Entry<WyilFile> source = (Path.Entry<WyilFile>) p.first();
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

	private JavaFile build(Path.Entry<WyilFile> source, Path.Entry<JavaFile> target) throws IOException {
		// Read the source file, which forces it to be parsed, etc.
		WyilFile wf = source.read();
		// Reset the list of unnamed types
		unnamedTypes = new HashMap<>();
		// Create an (empty) output file to contains the generated Java source
		// code.
		JavaFile jf = new JavaFile(target);

		// Add package declaration?

		// Add imports
		jf.getDeclarations().add(new JavaFile.Import("java","math","BigInteger"));
		jf.getDeclarations().add(new JavaFile.Import("java","util","Arrays"));

		String className = wf.getEntry().id().last();
		JavaFile.Class jcd = new JavaFile.Class(className);
		jcd.getModifiers().add(JavaFile.Modifier.PUBLIC);
		jcd.getModifiers().add(JavaFile.Modifier.FINAL);
		// Translate all declarations
		for (WyilFile.Block b : wf.blocks()) {
			try {
			if (b instanceof WyilFile.Declaration) {
				WyilFile.Declaration wd = (WyilFile.Declaration) b;

				// writeLocationsAsComments(d.getTree());

				if (wd instanceof WyilFile.FunctionOrMethod) {
					build((WyilFile.FunctionOrMethod) b, jcd);
				} else if (b instanceof WyilFile.Type) {
					build((WyilFile.Type) b, jcd);
				}
			}
			} catch(ResolveError e) {
				throw new IllegalArgumentException(e);
			}
		}
		// Add all generated unnamed types
		for(Map.Entry<Type, JavaFile.Class> e : unnamedTypes.entrySet()) {
			jcd.getDeclarations().add(e.getValue());
		}
		//
		jf.getDeclarations().add(jcd);
		return jf;
	}

	private void build(WyilFile.Type decl, JavaFile.Class parent) throws ResolveError {
		Type type = decl.type();

		// FIXME: deal with Nominal types ?

		if (type instanceof Type.Record) {
			//
			List<JavaFile.Field> fields = translateFieldDeclarations((Type.Record) type);
			JavaFile.Class struct = JavaCodeGenerator.generateStruct(decl.name(), fields);
			struct.getModifiers().addAll(translateModifiers(decl.modifiers()));
			parent.getDeclarations().add(struct);
		} else if(type instanceof Type.Union) {
			Type.Union ut = (Type.Union) type;
			JavaFile.Type[] bounds = translateTypes(ut.bounds());
			JavaFile.Class union = JavaCodeGenerator.generateUnion(decl.name(), bounds);
			union.getModifiers().addAll(translateModifiers(decl.modifiers()));
			parent.getDeclarations().add(union);
		}
	}

	private void build(WyilFile.FunctionOrMethod decl, JavaFile.Class parent) {
		SyntaxTree tree = decl.getTree();
		Type.FunctionOrMethod ft = decl.type();
		JavaFile.Type returnType = translateReturnTypes(ft.returns());
		//
		JavaFile.Method method = new JavaFile.Method(decl.name(), returnType);
		method.getModifiers().addAll(translateModifiers(decl.modifiers()));
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		//
		for (int i = 0; i != ft.params().length; ++i) {
			Location<VariableDeclaration> pd = (Location<VariableDeclaration>) tree.getLocation(i);
			JavaFile.Type pt = translateType(ft.parameter(i));
			method.getParameters().add(new Pair<>(pt, pd.getBytecode().getName()));
		}
		//
		// FIXME: should provide support for checking preconditions /
		// postconditions here. These can be implemented as runtime checks.
		//
		method.setBody(translateBlock(decl.getBody()));
		parent.getDeclarations().add(method);
	}

	private JavaFile.Block translateBlock(Location<Bytecode.Block> block) {
		JavaFile.Block jblock = new JavaFile.Block();
		for (Location<?> term : block.getOperands()) {
			JavaFile.Term jterm = translateStatement(term);
			jblock.getTerms().add(jterm);
		}
		return jblock;
	}

	private JavaFile.Term translateStatement(Location<?> c) {
		try {
			switch (c.getOpcode()) {
			case Bytecode.OPCODE_aliasdecl:
				// return
				// translateAliasDeclaration((Location<Bytecode.AliasDeclaration>)
				// c);
			case Bytecode.OPCODE_assert:
				return translateAssert((Location<Bytecode.Assert>) c);
			case Bytecode.OPCODE_assume:
				return translateAssume((Location<Bytecode.Assume>) c);
			case Bytecode.OPCODE_assign:
				return translateAssign((Location<Bytecode.Assign>) c);
			case Bytecode.OPCODE_break:
				return translateBreak((Location<Bytecode.Break>) c);
			case Bytecode.OPCODE_continue:
				return translateContinue((Location<Bytecode.Continue>) c);
			//case Bytecode.OPCODE_debug:
				// return translateDebug((Location<Bytecode.Debug>) c);
			case Bytecode.OPCODE_dowhile:
				 return translateDoWhile((Location<Bytecode.DoWhile>) c);
			//case Bytecode.OPCODE_fail:
				// return translateFail((Location<Bytecode.Fail>) c);
			case Bytecode.OPCODE_if:
			case Bytecode.OPCODE_ifelse:
				return translateIf((Location<Bytecode.If>) c);
			//case Bytecode.OPCODE_indirectinvoke:
				// return
				// translateIndirectInvoke((Location<Bytecode.IndirectInvoke>)
				// c);
			case Bytecode.OPCODE_invoke:
				return translateInvoke((Location<Bytecode.Invoke>) c);
//			case Bytecode.OPCODE_namedblock:
//				 return translateBlock((Location<Bytecode.NamedBlock>) c);
			case Bytecode.OPCODE_while:
				return translateWhile((Location<Bytecode.While>) c);
			case Bytecode.OPCODE_return:
				return translateReturn((Location<Bytecode.Return>) c);
			case Bytecode.OPCODE_switch:
				return translateSwitch((Location<Bytecode.Switch>) c);
			case Bytecode.OPCODE_vardecl:
			case Bytecode.OPCODE_vardeclinit:
				return translateVariableDeclaration((Location<Bytecode.VariableDeclaration>) c);
			default:
				throw new IllegalArgumentException("unknown bytecode encountered");
			}
		} catch (ResolveError e) {
			throw new RuntimeException("resolve error", e);
		}
	}

	private JavaFile.Term translateAssert(Location<Bytecode.Assert> stmt) {
		JavaFile.Term operand = translateExpression(stmt.getOperand(0));
		return new JavaFile.Assert(operand);
	}

	private JavaFile.Term translateAssign(Location<Bytecode.Assign> stmt) {
		JavaFile.Term[] lhs = translateExpressions(stmt.getOperandGroup(LEFTHANDSIDE));
		JavaFile.Term[] rhs = translateExpressions(stmt.getOperandGroup(RIGHTHANDSIDE));
		if(lhs.length > 1) {
			throw new IllegalArgumentException("Need support for multiple assignments");
		} else {
			return new JavaFile.Assignment(lhs[0],rhs[0]);
		}
	}

	private JavaFile.Term translateAssume(Location<Bytecode.Assume> stmt) {
		JavaFile.Term operand = translateExpression(stmt.getOperand(0));
		return new JavaFile.Assert(operand);
	}

	private JavaFile.Term translateBreak(Location<Bytecode.Break> stmt) {
		return new JavaFile.Break();
	}

	private JavaFile.Term translateContinue(Location<Bytecode.Continue> stmt) {
		return new JavaFile.Continue();
	}

	private JavaFile.Term translateDoWhile(Location<Bytecode.DoWhile> stmt) {
		JavaFile.Term condition = translateExpression(stmt.getOperand(0));
		JavaFile.Block body = translateBlock(stmt.getBlock(0));
		return new JavaFile.DoWhile(body, condition);
	}

	private JavaFile.Term translateIf(Location<Bytecode.If> stmt) {
		JavaFile.Term operand = translateExpression(stmt.getOperand(0));
		JavaFile.Block trueBranch = translateBlock(stmt.getBlock(0));
		JavaFile.Block falseBranch = null;
		if (stmt.numberOfBlocks() > 1) {
			falseBranch = translateBlock(stmt.getBlock(1));
		}
		return new JavaFile.If(operand, trueBranch, falseBranch);
	}

	private JavaFile.Term translateVariableDeclaration(Location<Bytecode.VariableDeclaration> stmt) {
		Bytecode.VariableDeclaration d = stmt.getBytecode();
		JavaFile.Type type = translateType(stmt.getType());
		JavaFile.Term initialiser = null;
		if (stmt.numberOfOperands() > 0) {
			initialiser = translateExpression(stmt.getOperand(0));
		}
		return new JavaFile.VariableDeclaration(type, d.getName(), initialiser);
	}

	private JavaFile.Term translateReturn(Location<Bytecode.Return> stmt) {
		JavaFile.Term initialiser = null;
		if (stmt.numberOfOperands() > 0) {
			initialiser = translateExpression(stmt.getOperand(0));
		}
		return new JavaFile.Return(initialiser);
	}

	private JavaFile.Term translateSwitch(Location<Bytecode.Switch> stmt) throws ResolveError {
		JavaFile.Term condition = translateExpression(stmt.getOperand(0));
		Bytecode.Switch bytecode = stmt.getBytecode();
		Bytecode.Case[] cases = bytecode.cases();
		ArrayList<JavaFile.Case> jCases = new ArrayList<>();
		//
		for (int i = 0; i != cases.length; ++i) {
			Bytecode.Case c = cases[i];
			for (Constant v : c.values()) {
				// FIXME: problem here when values are not constants
				JavaFile.Constant label = (JavaFile.Constant) translateConstant(v);
				JavaFile.Block body = translateBlock(stmt.getBlock(i));
				jCases.add(new JavaFile.Case(label, body));
			}
		}
		//
		return new JavaFile.Switch(condition, jCases);
	}

	private JavaFile.Term translateWhile(Location<Bytecode.While> stmt) {
		JavaFile.Term condition = translateExpression(stmt.getOperand(0));
		JavaFile.Block body = translateBlock(stmt.getBlock(0));
		return new JavaFile.While(condition, body);
	}

	private void writeLocationsAsComments(SyntaxTree tree) {
		List<Location<?>> locations = tree.getLocations();
		for (int i = 0; i != locations.size(); ++i) {
			Location<?> loc = locations.get(i);
			String id = String.format("%1$" + 3 + "s", "#" + i);
			String type = String.format("%1$-" + 8 + "s", Arrays.toString(loc.getTypes()));
			System.out.println("// " + id + " " + type + " " + loc.getBytecode());
		}
	}

	private JavaFile.Term[] translateExpressions(Location<?>[] expr) {
		JavaFile.Term[] terms = new JavaFile.Term[expr.length];
		for(int i=0;i!=terms.length;++i) {
			terms[i] = translateExpression(expr[i]);
		}
		return terms;
	}

	// @SuppressWarnings("unchecked")
	private JavaFile.Term translateExpression(Location<?> expr) {
		try {
			switch (expr.getOpcode()) {
			case Bytecode.OPCODE_add:
			case Bytecode.OPCODE_sub:
			case Bytecode.OPCODE_mul:
			case Bytecode.OPCODE_div:
			case Bytecode.OPCODE_rem:
			case Bytecode.OPCODE_eq:
			case Bytecode.OPCODE_ne:
			case Bytecode.OPCODE_lt:
			case Bytecode.OPCODE_le:
			case Bytecode.OPCODE_gt:
			case Bytecode.OPCODE_ge:
			case Bytecode.OPCODE_logicaland:
			case Bytecode.OPCODE_logicalor:
			case Bytecode.OPCODE_bitwiseor:
			case Bytecode.OPCODE_bitwisexor:
			case Bytecode.OPCODE_bitwiseand:
			case Bytecode.OPCODE_shl:
			case Bytecode.OPCODE_shr:
			case Bytecode.OPCODE_is:
			case Bytecode.OPCODE_dereference:
			case Bytecode.OPCODE_logicalnot:
			case Bytecode.OPCODE_neg:
			case Bytecode.OPCODE_newobject:
			case Bytecode.OPCODE_bitwiseinvert:
				return translateOperator((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_arraylength:
				return translateArrayLength((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_arrayindex:
				return translateArrayAccess((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_array:
				return translateArrayInitialiser((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_arraygen:
				return translateArrayGenerator((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_convert:
				return translateConvert((Location<Bytecode.Convert>) expr);
			case Bytecode.OPCODE_const:
				return translateConst((Location<Bytecode.Const>) expr);
			case Bytecode.OPCODE_fieldload:
				return translateFieldLoad((Location<Bytecode.FieldLoad>) expr);
			case Bytecode.OPCODE_indirectinvoke:
				return translateIndirectInvoke((Location<Bytecode.IndirectInvoke>) expr);
			case Bytecode.OPCODE_invoke:
				return translateInvoke((Location<Bytecode.Invoke>) expr);
			case Bytecode.OPCODE_lambda:
				return translateLambda((Location<Bytecode.Lambda>) expr);
			case Bytecode.OPCODE_record:
				return translateRecordInitialiser((Location<Bytecode.Operator>) expr);
			case Bytecode.OPCODE_all:
			case Bytecode.OPCODE_some:
				return translateQuantifier((Location<Bytecode.Quantifier>) expr);
			case Bytecode.OPCODE_varmove:
			case Bytecode.OPCODE_varcopy:
				return translateVariableAccess((Location<Bytecode.VariableAccess>) expr);
			default:
				throw new IllegalArgumentException("unknown bytecode encountered: " + expr.getBytecode());
			}
		} catch (ResolveError e) {
			throw new RuntimeException("resolve error", e);
		}
	}

	private JavaFile.Term translateOperator(Location<Bytecode.Operator> expr) throws ResolveError {
		Bytecode.OperatorKind kind = expr.getBytecode().kind();
		// First, translate all children of the operator.
		List<JavaFile.Term> children = new ArrayList<>();
		for (int i = 0; i != expr.numberOfOperands(); ++i) {
			children.add(translateExpression(expr.getOperand(i)));
		}
		//
		Type argumentType = expr.getOperand(0).getType();
		if (isDynamicallySized(argumentType)) {
			// In this case, we have dynamically-sized arguments (e.g.
			// BigInteger). In such case, we must exploit the various methods on
			// Object or BigInteger for evaluating the operation.

			// FIXME: this coercion should be deprecated with proper support for
			// fixed-size types in the Whiley Compiler.
			for (int i = 0; i != children.size(); ++i) {
				children.set(i, toBigInteger(children.get(i), expr.getOperand(i).getType()));
			}
			//
			switch (kind) {
			case NEG:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
				return translateUnboundArithmeticOperator(kind, children);
			case EQ:
			case NEQ:
			case LT:
			case LTEQ:
			case GT:
			case GTEQ:
				return translateUnboundComparator(kind, children);
			default:
				throw new IllegalArgumentException("Unknown expression encountered");
			}
		} else {
			// In this case, we have a fixed-sized arguments. Therefore, we can
			// employ Java's underlying arithmetic operators, comparators, etc.
			JavaFile.Operator.Kind k = translate2JavaOperator(kind);
			// Apply any necessary coercions
			switch(kind) {
			case LEFTSHIFT:
			case RIGHTSHIFT: {
				// FIXME: in principle, this should be unnecesssary as the
				// WhileyCompiler should take care of this.
				children.set(1,toInt(children.get(1),expr.getOperand(1).getType()));
			}
			case BITWISEXOR:
			case BITWISEOR:
			case BITWISEAND: {
				JavaFile.Term result = new JavaFile.Operator(k, children);
				return new JavaFile.Cast(JavaFile.BYTE, result);
			}

			}
			return new JavaFile.Operator(k, children);
		}
	}

	/**
	 * Translate an unbound arithmetic operator. For example, the following
	 * Whiley code:
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
	 * @param k
	 * @param operands
	 * @return
	 */
	private static JavaFile.Term translateUnboundArithmeticOperator(Bytecode.OperatorKind k,
			List<JavaFile.Term> operands) {
		String methodName;
		switch (k) {
		case NEG:
			methodName = "negate";
			break;
		case ADD:
			methodName = "add";
			break;
		case SUB:
			methodName = "subtract";
			break;
		case MUL:
			methodName = "multiply";
			break;
		case DIV:
			methodName = "divide";
			break;
		case REM:
			methodName = "remainder";
			break;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + k);
		}
		// Construct the method invocation
		JavaFile.Term receiver = operands.get(0);
		if (operands.size() == 1) {
			return new JavaFile.Invoke(receiver, new String[] { methodName });
		} else {
			JavaFile.Term operand = operands.get(1);
			return new JavaFile.Invoke(receiver, new String[] { methodName }, operand);
		}
	}

	private static JavaFile.Term translateUnboundComparator(Bytecode.OperatorKind k, List<JavaFile.Term> operands) {
		JavaFile.Operator.Kind kind;
		JavaFile.Term receiver = operands.get(0);
		JavaFile.Term operand = operands.get(1);
		//
		switch (k) {
		case NEQ: {
			JavaFile.Term eq = new JavaFile.Invoke(receiver, new String[] { "equals" }, operand);
			return new JavaFile.Operator(JavaFile.Operator.Kind.NOT, eq);
		}
		case EQ: {
			return new JavaFile.Invoke(receiver, new String[] { "equals" }, operand);
		}
		case LT:
			kind = JavaFile.Operator.Kind.LT;
			break;
		case LTEQ:
			kind = JavaFile.Operator.Kind.LTEQ;
			break;
		case GT:
			kind = JavaFile.Operator.Kind.GT;
			break;
		case GTEQ:
			kind = JavaFile.Operator.Kind.GTEQ;
			break;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + k);
		}
		// Construct the method invocation
		JavaFile.Term cmp = new JavaFile.Invoke(receiver, new String[] { "compareTo" }, operand);
		return new JavaFile.Operator(kind, cmp, new JavaFile.Constant(0));
	}

	private JavaFile.Term translateArrayAccess(Location<Bytecode.Operator> expr) throws ResolveError {
		Location<?> indexExpr = expr.getOperand(1);
		JavaFile.Term src = translateExpression(expr.getOperand(0));
		JavaFile.Term index = translateExpression(indexExpr);
		return new JavaFile.ArrayAccess(src, toInt(index, indexExpr.getType()));
	}

	private JavaFile.Term translateArrayGenerator(Location<Bytecode.Operator> expr) {
		throw new IllegalArgumentException("IMPLEMENT ME");
	}

	private JavaFile.Term translateArrayInitialiser(Location<Bytecode.Operator> expr) {
		List<JavaFile.Term> children = new ArrayList<>();
		for (int i = 0; i != expr.numberOfOperands(); ++i) {
			children.add(translateExpression(expr.getOperand(i)));
		}
		JavaFile.Array type = (JavaFile.Array) translateType(expr.getType());
		return new JavaFile.NewArray(type, null, children);
	}

	private JavaFile.Term translateArrayLength(Location<Bytecode.Operator> expr) throws ResolveError {
		JavaFile.Term src = translateExpression(expr.getOperand(0));
		// FIXME: converting the array length to a big integer is a temporary
		// fix. It works around the fact that the Whiley compiler types the
		// return of an array length expression as just "int", when in fact it
		// should be: "usize".
		return toBigInteger(new JavaFile.FieldAccess(src, "length"), TYPE_I32);
	}

	private JavaFile.Term translateConvert(Location<Bytecode.Convert> expr) {
		// out.print("(");
		// writeType(expr.getType());
		// out.print(") ");
		// writeExpression(expr.getOperand(0));
		return null;
	}

	private JavaFile.Term translateConst(Location<Bytecode.Const> expr) throws ResolveError {
		Constant c = expr.getBytecode().constant();
		return translateConstant(c);
	}

	private JavaFile.Term translateConstant(Constant c) throws ResolveError {
		Type type = c.type();
		Object value;
		if (c instanceof Constant.Null) {
			value = null;
		} else if (c instanceof Constant.Bool) {
			Constant.Bool bc = (Constant.Bool) c;
			value = bc.value();
		} else if (c instanceof Constant.Byte) {
			Constant.Byte bc = (Constant.Byte) c;
			value = bc.value();
		} else if (c instanceof Constant.Integer) {
			Constant.Integer bc = (Constant.Integer) c;
			BigInteger bi = bc.value();
			// This case is more complex because we have to manage large integer
			// values (which do not fit as literals).
			if (isDynamicallySized(type)) {
				return translateUnboundIntegerConstant(bi);
			} else {
				return translateFixedIntegerConstant(bi);
			}
		} else if (c instanceof Constant.Array) {
			Constant.Array ac = (Constant.Array) c;
			return translateArrayConstant((Constant.Array) c);
		} else {
			throw new IllegalArgumentException("GOT HERE");
		}
		return new JavaFile.Constant(value);
	}

	/**
	 * Translate an integer constant which will be assigned to a fixed-sized
	 * Java variable. Such constants can be safely expressed as integer
	 * literals.
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
	 * Translate an integer constant which will be assigned to a
	 * dynamically-sized Java variable. Such constants cannot be expressed as
	 * integer literals and must be converted into instances of
	 * <code>BigInteger</code>.
	 *
	 * @param constant
	 * @return
	 */
	private JavaFile.Term translateUnboundIntegerConstant(BigInteger constant) {
		long lv = constant.longValue();
		// FIXME: bug here for constants which cannot fit inside a long
		return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, new JavaFile.Constant(lv));
	}

	/**
	 * Translate an array constant. This simply creates an array of the
	 * appropriate type and employs an initialiser expression to assign each
	 * element. For example, the following Whiley code:
	 *
	 * <pre>
	 * int[] xs = [1,2]
	 * </pre>
	 *
	 * is translated into the following Java code
	 *
	 * <pre>
	 * BigInteger[] xs = new BigInteger[] { BigInteger.valueOf(1), BigInteger.valueOf(1) };
	 * </pre>
	 *
	 * @param array
	 * @return
	 * @throws ResolveError
	 */
	private JavaFile.Term translateArrayConstant(Constant.Array array) throws ResolveError {
		List<Constant> elements = array.values();
		List<JavaFile.Term> children = new ArrayList<>();
		for (int i = 0; i != elements.size(); ++i) {
			children.add(translateConstant(elements.get(i)));
		}
		JavaFile.Array type = (JavaFile.Array) translateType(array.type());
		return new JavaFile.NewArray(type, null, children);
	}

	private JavaFile.Term translateFieldLoad(Location<Bytecode.FieldLoad> expr) {
		Bytecode.FieldLoad fld = expr.getBytecode();
		JavaFile.Term source = translateExpression(expr.getOperand(0));
		return new JavaFile.FieldAccess(source, fld.fieldName());
	}

	private JavaFile.Term translateIndirectInvoke(Location<Bytecode.IndirectInvoke> expr) {
		// Location<?>[] operands = expr.getOperands();
		// writeExpression(operands[0]);
		// out.print("(");
		// for (int i = 1; i != operands.length; ++i) {
		// if (i != 1) {
		// out.print(", ");
		// }
		// writeExpression(operands[i]);
		// }
		// out.print(")");
		return null;
	}

	private JavaFile.Term translateInvoke(Location<Bytecode.Invoke> expr) {
		Location<?>[] operands = expr.getOperands();
		List<String> path = new ArrayList<>();
		List<JavaFile.Term> arguments = new ArrayList<>();
		path.add(expr.getBytecode().name().name());
		for (int i = 0; i != operands.length; ++i) {
			arguments.add(translateExpression(operands[i]));
		}
		return new JavaFile.Invoke(null, path, arguments);
	}

	@SuppressWarnings("unchecked")
	private JavaFile.Term translateLambda(Location<Bytecode.Lambda> expr) {
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

	private JavaFile.Term translateRecordInitialiser(Location<Bytecode.Operator> expr) throws ResolveError {
		Type type = expr.getType();
		JavaFile.Reference jType;
		//
		if (type instanceof Type.Nominal) {
			Type.Nominal nt = (Type.Nominal) type;
			jType = new JavaFile.Reference(nt.name().name());
		} else if(type instanceof Type.Record) {
			JavaFile.Class unnamedStruct = generateAnonymousStruct((Type.Record) type);
			jType = new JavaFile.Reference(unnamedStruct.getName());
		} else {
			throw new IllegalArgumentException("Not sure what to do here");
		}
		// Construct the appropriate record now
		Location<?>[] operands = expr.getOperands();
		ArrayList<JavaFile.Term> parameters = new ArrayList<>();
		for (int i = 0; i != operands.length; ++i) {
			parameters.add(translateExpression(expr.getOperand(i)));
		}
		return new JavaFile.New(jType, parameters);
	}

	@SuppressWarnings("unchecked")
	private JavaFile.Term translateQuantifier(Location<Bytecode.Quantifier> c) {
		// out.print(quantifierKind(c));
		// out.print(" { ");
		// for (int i = 0; i != c.numberOfOperandGroups(); ++i) {
		// Location<?>[] range = c.getOperandGroup(i);
		// if (i != 0) {
		// out.print(", ");
		// }
		// Location<VariableDeclaration> v = (Location<VariableDeclaration>)
		// range[SyntaxTree.VARIABLE];
		// out.print(v.getBytecode().getName());
		// out.print(" in ");
		// writeExpression(range[SyntaxTree.START]);
		// out.print("..");
		// writeExpression(range[SyntaxTree.END]);
		// }
		// out.print(" | ");
		// writeExpression(c.getOperand(SyntaxTree.CONDITION));
		// out.print(" } ");
		return null;
	}

	private JavaFile.Term translateVariableAccess(Location<Bytecode.VariableAccess> expr) {
		Location<VariableDeclaration> vd = getVariableDeclaration(expr.getOperand(0));
		JavaFile.Term t = new JavaFile.VariableAccess(vd.getBytecode().getName());
		JavaFile.Type type = translateType(expr.getType());
		if (expr.getOpcode() == Bytecode.OPCODE_varcopy && !isCopyable(type)) {
			// Since this type is not copyable, we need to clone it to ensure
			// that ownership is properly preserved.
			t = new JavaFile.Invoke(t, "clone");
		}
		return t;
	}

	private static List<JavaFile.Modifier> translateModifiers(List<Modifier> modifiers) {
		ArrayList<JavaFile.Modifier> javaModifiers = new ArrayList<>();
		for (Modifier m : modifiers) {
			if (m == Modifier.PUBLIC) {
				javaModifiers.add(JavaFile.Modifier.PUBLIC);
			} else if (m == Modifier.PRIVATE) {
				javaModifiers.add(JavaFile.Modifier.PRIVATE);
			}
		}
		return javaModifiers;
	}

	/**
	 * Convert a sequence of zero or more types which represent the return
	 * values of a function or method. In the case of exactly one type, then it
	 * just returns the conversion of that type. Otherwise, it returns the type
	 * of an object array which will hold the necessary arguments.
	 *
	 * @param types
	 * @return
	 */
	private JavaFile.Type translateReturnTypes(Type... types) {
		if (types.length == 0) {
			return JavaFile.VOID;
		} else if (types.length == 1) {
			return translateType(types[0]);
		} else {
			throw new RuntimeException("Got here");
		}
	}

	/**
	 * Convert a Whiley type into its equivalent Java form. For example, the
	 * Whiley type <code>int:32</code> corresponds to the Java type
	 * <code>int</code>. Some conversions are <i>lossy</i>, meaning that we lose
	 * information during the conversion. For example, the Whiley union type
	 * <code>int|bool</code> is reduced to just <code>Object</code> in Java.
	 *
	 * @param type
	 */
	private JavaFile.Type translateType(Type type) {
		JavaFile.Type jtype = typeMap.get(type);
		if (jtype != null) {
			return jtype;
		} else if (type instanceof Type.Array) {
			Type.Array arrT = (Type.Array) type;
			return new JavaFile.Array(translateType(arrT.element()));
		} else if (type instanceof Type.Nominal) {
			try {
				Type.Nominal tn = (Type.Nominal) type;
				NameID nid = tn.name();
				WyilFile f = project.get(nid.module(), WyilFile.ContentType).read();
				WyilFile.Type decl = f.type(nid.name());
				if (decl.type() instanceof Type.Record) {
					return new JavaFile.Reference(tn.name().name());
				} else {
					return translateType(decl.type());
				}
			} catch (IOException e) {
				throw new IllegalArgumentException(e);
			}
		} else if (type instanceof Type.Record) {
			JavaFile.Class struct = generateAnonymousStruct((Type.Record)type);
			return new JavaFile.Reference(struct.getName());
		} else {
			// Default
			return new JavaFile.Reference("Object");
		}
	}

	private JavaFile.Type[] translateTypes(Type... types) {
		JavaFile.Type[] rs = new JavaFile.Type[types.length];
		for(int i=0;i!=types.length;++i) {
			rs[i] = translateType(types[i]);
		}
		return rs;
	}
	private JavaFile.Class generateAnonymousStruct(Type.Record type) {
		JavaFile.Class r = unnamedTypes.get(type);
		if(r == null) {
			List<JavaFile.Field> fields = translateFieldDeclarations(type);
			r = JavaCodeGenerator.generateStruct("Struct" + unnamedTypes.size(), fields);
			unnamedTypes.put(type, r);
		}
		return r;
	}

	private List<JavaFile.Field> translateFieldDeclarations(Type.Record type) {
		ArrayList<JavaFile.Field> fields = new ArrayList<>();
		String[] fieldNames = type.getFieldNames();
		for (int i = 0; i != fieldNames.length; ++i) {
			String fieldName = fieldNames[i];
			JavaFile.Type fieldType = translateType(type.getField(fieldName));
			fields.add(new JavaFile.Field(fieldType, fieldName));
		}
		return fields;
	}

	public boolean isDynamicallySized(Type type) throws ResolveError {
		// FIXME: this is basically completely broken.
		if (type == TYPE_I8 || type == TYPE_I16 || type == TYPE_I32 || type == TYPE_I64 || type == TYPE_U8
				|| type == TYPE_U16 || type == TYPE_U32 || type == TYPE_U64) {
			return false;
		} else if (typeSystem.isSubtype(Type.T_INT, type)) {
			return true;
		} else if (typeSystem.expandAsEffectiveArray(type) != null) {
			return true;
		} else {
			// FIXME: need to recursively check component types for records.
			return false;
		}
	}

	private static Type TYPE_I8 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "i8"));
	private static Type TYPE_I16 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "i16"));
	private static Type TYPE_I32 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "i32"));
	private static Type TYPE_I64 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "i64"));
	private static Type TYPE_U8 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "u8"));
	private static Type TYPE_U16 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "u16"));
	private static Type TYPE_U32 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "u32"));
	private static Type TYPE_U64 = Type.Nominal(new NameID(Trie.fromString("whiley/lang/Int"), "u64"));

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
			//
			put(Type.T_BOOL, JavaFile.BOOLEAN);
			put(Type.T_BYTE, JavaFile.BYTE);
			put(Type.T_INT, JAVA_MATH_BIGINTEGER);
			put(Type.T_ANY, new JavaFile.Reference("Object"));
		}
	};

	/**
	 * Convert a given term which returns a value of integer type into a Java
	 * int. Essentially, if the returned value is a BigInteger then this will
	 * invoke <code>BigInteger.intValue()</code>. Otherwise, it's a
	 * no-operation.
	 *
	 * @param term
	 *            The Java term whose value is being converted to an
	 *            <code>int</code>.
	 * @param type
	 *            The Whiley type associated with the given term.
	 * @return
	 * @throws ResolveError
	 */
	private JavaFile.Term toInt(JavaFile.Term term, Type type) throws ResolveError {
		if (isDynamicallySized(type)) {
			return new JavaFile.Invoke(term, new String[] { "intValue" });
		} else {
			return term;
		}
	}

	/**
	 * Convert a given term which returns a value of Java int or long type into
	 * a Java BigInteger. Essentially, if the returned value is a such a type
	 * then this will invoke <code>BigInteger.valueOf()</code>. Otherwise, it's
	 * a no-operation.
	 *
	 * @param term
	 *            The Java term whose value is being converted to a
	 *            <code>BigInteger</code>.
	 * @param type
	 *            The Whiley type associated with the given term.
	 * @return
	 * @throws ResolveError
	 */
	private JavaFile.Term toBigInteger(JavaFile.Term term, Type type) throws ResolveError {
		if (isDynamicallySized(type)) {
			return term;
		} else {
			return new JavaFile.Invoke(null, new String[] { "BigInteger", "valueOf" }, term);
		}
	}

	/**
	 * Return true if the type in question can be copied directly. More
	 * specifically, if a bitwise copy of the value is sufficient to fully copy
	 * it. In general, this is true for primitive data types in Java. But, for
	 * array types or general class types, it is not true (since these are
	 * references into the heap). As an exception, class types which are known
	 * to be immutable can be safely considered as copyable.
	 *
	 * @param type
	 * @return
	 */
	private static boolean isCopyable(JavaFile.Type type) {
		if (type instanceof JavaFile.Primitive || type == JAVA_MATH_BIGINTEGER) {
			return true;
		} else {
			// FIXME: could do better here, e.g. for immutable reference types
			return false;
		}
	}

	private static JavaFile.Operator.Kind translate2JavaOperator(Bytecode.OperatorKind k) {
		switch (k) {
		case NEG:
			return JavaFile.Operator.Kind.NEG;
		case NOT:
			return JavaFile.Operator.Kind.NOT;
		// Binary
		case ADD:
			return JavaFile.Operator.Kind.ADD;
		case SUB:
			return JavaFile.Operator.Kind.SUB;
		case MUL:
			return JavaFile.Operator.Kind.MUL;
		case DIV:
			return JavaFile.Operator.Kind.DIV;
		case REM:
			return JavaFile.Operator.Kind.REM;
		case EQ:
			return JavaFile.Operator.Kind.EQ;
		case NEQ:
			return JavaFile.Operator.Kind.NEQ;
		case LT:
			return JavaFile.Operator.Kind.LT;
		case LTEQ:
			return JavaFile.Operator.Kind.LTEQ;
		case GT:
			return JavaFile.Operator.Kind.GT;
		case GTEQ:
			return JavaFile.Operator.Kind.GTEQ;
		case AND:
			return JavaFile.Operator.Kind.AND;
		case OR:
			return JavaFile.Operator.Kind.OR;
		case BITWISEOR:
			return JavaFile.Operator.Kind.BITWISEOR;
		case BITWISEXOR:
			return JavaFile.Operator.Kind.BITWISEXOR;
		case BITWISEAND:
			return JavaFile.Operator.Kind.BITWISEAND;
		case BITWISEINVERT:
			return JavaFile.Operator.Kind.BITWISEINVERT;
		case LEFTSHIFT:
			return JavaFile.Operator.Kind.LEFTSHIFT;
		case RIGHTSHIFT:
			return JavaFile.Operator.Kind.RIGHTSHIFT;
		default:
			throw new IllegalArgumentException("unknown operator kind : " + k);
		}
	}

	@SuppressWarnings("unchecked")
	private Location<VariableDeclaration> getVariableDeclaration(Location<?> loc) {
		switch (loc.getOpcode()) {
		case Bytecode.OPCODE_vardecl:
		case Bytecode.OPCODE_vardeclinit:
			return (Location<VariableDeclaration>) loc;
		case Bytecode.OPCODE_aliasdecl:
			return getVariableDeclaration(loc.getOperand(0));
		}
		throw new IllegalArgumentException("invalid location provided: " + loc);
	}
}
