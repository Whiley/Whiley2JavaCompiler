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
import wybs.lang.Build.Graph;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Path;
import wyfs.lang.Path.Entry;
import wyfs.lang.Path.Root;
import wyfs.util.Trie;
import wyil.lang.*;
import wyil.lang.Bytecode.VariableDeclaration;
import wyil.lang.SyntaxTree.Location;
import wyil.util.TypeSystem;
import wyjc.core.JavaFile;

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
		// Create an (empty) output file to contains the generated Java source
		// code.
		JavaFile jf = new JavaFile(target);

		// Add package declaration
		// Add imports

		String className = wf.getEntry().id().last();
		JavaFile.Class jcd = new JavaFile.Class(className);
		jcd.getModifiers().add(JavaFile.Modifier.PUBLIC);
		jcd.getModifiers().add(JavaFile.Modifier.FINAL);
		// Translate all declarations
		for (WyilFile.Block b : wf.blocks()) {
			if (b instanceof WyilFile.Declaration) {
				WyilFile.Declaration wd = (WyilFile.Declaration) b;
				;
				// writeLocationsAsComments(d.getTree());

				if (wd instanceof WyilFile.FunctionOrMethod) {
					build((WyilFile.FunctionOrMethod) b, jcd);
				} else if (b instanceof WyilFile.Type) {
					build((WyilFile.Type) b, jcd);
				}
			}
		}
		jf.getDeclarations().add(jcd);
		return jf;
	}

	private void build(WyilFile.Type decl, JavaFile.Class parent) {
		Type type = decl.type();

		// FIXME: expand nominal types

		if (type instanceof Type.Record) {
			Type.Record recT = (Type.Record) type;
			JavaFile.Class typeClass = new JavaFile.Class(decl.name());
			typeClass.getModifiers().add(JavaFile.Modifier.PUBLIC);
			typeClass.getModifiers().add(JavaFile.Modifier.STATIC);
			typeClass.getModifiers().add(JavaFile.Modifier.FINAL);
			// Write field declartions
			// writeFieldDeclarations(indent,recT);
			// writeRecordConstructor(indent,decl.name(),recT);
			// writeRecordEquals(indent,decl.name(),recT);
			// writeRecordHashCode(indent,decl.name(),recT);
			// writeRecordClone(indent,decl.name(),recT);
			// tabIndent(indent);
			parent.getDeclarations().add(typeClass);
		}
	}

	// private void writeFieldDeclarations(int indent, Type.Record recT) {
	// String[] fields = recT.getFieldNames();
	// for(int i=0;i!=recT.size();++i) {
	// String field = fields[i];
	// tabIndent(indent+1);
	// out.print("public ");
	// writeType(recT.getField(field));
	// out.println(" " + field + ";");
	// }
	// out.println();
	// }
	//
	// private void writeRecordConstructor(int indent, String name, Type.Record
	// recT) {
	// tabIndent(indent+1);
	// out.print("public ");
	// out.print(name);
	// out.print("(");
	// String[] fields = recT.getFieldNames();
	// for(int i=0;i!=recT.size();++i) {
	// if(i != 0) {
	// out.print(", ");
	// }
	// String field = fields[i];
	// writeType(recT.getField(field));
	// out.print(" " + field);
	// }
	// out.println(") {");
	// for(int i=0;i!=recT.size();++i) {
	// tabIndent(indent+2);
	// String field = fields[i];
	// out.println("this." + field + " = " + field + ";");
	// }
	// tabIndent(indent+1);
	// out.println("}");
	// out.println();
	// }
	//
	// private void writeRecordEquals(int indent, String name, Type.Record recT)
	// {
	// tabIndent(indent+1);
	// out.println("@Override");
	// tabIndent(indent+1);
	// out.println("public boolean equals(Object o) {");
	// tabIndent(indent+2);
	// out.println("if(o instanceof " + name + ") {");
	// tabIndent(indent+3);
	// out.println(name + " r = (" + name + ") o;");
	// tabIndent(indent+3);
	// out.print("return ");
	// String[] fields = recT.getFieldNames();
	// for(int i=0;i!=recT.size();++i) {
	// if(i != 0) {
	// out.print(" && ");
	// }
	// String field = fields[i];
	// out.print(field + " == r." + field);
	// }
	// out.println(";");
	// tabIndent(indent+2);
	// out.println("}");
	// tabIndent(indent+2);
	// out.println("return false;");
	// tabIndent(indent+1);
	// out.println("}");
	// out.println();
	// }
	//
	// private void writeRecordHashCode(int indent, String name, Type.Record
	// recT) {
	// tabIndent(indent+1);
	// out.println("@Override");
	// tabIndent(indent+1);
	// out.println("public int hashCode() {");
	// tabIndent(indent+2);
	// out.print("return ");
	// String[] fields = recT.getFieldNames();
	// for(int i=0;i!=recT.size();++i) {
	// if(i != 0) {
	// out.print(" ^ ");
	// }
	// String field = fields[i];
	// out.print(field);
	// }
	// out.println(";");
	// tabIndent(indent+1);
	// out.println("}");
	// out.println();
	// }
	//
	// private void writeRecordClone(int indent, String name, Type.Record recT)
	// {
	// tabIndent(indent+1);
	// out.println("@Override");
	// tabIndent(indent+1);
	// out.println("public Object clone() {");
	// tabIndent(indent+2);
	// out.print("return new " + name + "(");
	// String[] fields = recT.getFieldNames();
	// for(int i=0;i!=recT.size();++i) {
	// if(i != 0) {
	// out.print(",");
	// }
	// String field = fields[i];
	// out.print(field);
	// }
	// out.println(");");
	// tabIndent(indent+1);
	// out.println("}");
	// }

	private void build(WyilFile.FunctionOrMethod decl, JavaFile.Class parent) {
		SyntaxTree tree = decl.getTree();
		Type.FunctionOrMethod ft = decl.type();
		JavaFile.Type returnType = translateReturnTypes(ft.returns());
		//
		JavaFile.Method method = new JavaFile.Method(decl.name(),returnType);
		addModifiers(method, decl.modifiers());
		//
		for(int i=0;i!=ft.params().length;++i) {
			Location<VariableDeclaration> pd = (Location<VariableDeclaration>) tree.getLocation(i);
			JavaFile.Type pt = translateType(ft.parameter(i));
			method.getParameters().add(new Pair<>(pt,pd.getBytecode().getName()));
		}
		//
		// FIXME: preconditions / postconditions?
		//
		method.setBody(translateBlock(decl.getBody()));
		parent.getDeclarations().add(method);
	}

	private JavaFile.Block translateBlock(Location<Bytecode.Block> block) {
		JavaFile.Block jblock = new JavaFile.Block();
		for(Location<?> term : block.getOperands()) {
			JavaFile.Term jterm = translateStatement(term);
			jblock.getTerms().add(jterm);
		}
		return jblock;
	}

	private JavaFile.Term translateStatement(Location<?> c) {
		switch (c.getOpcode()) {
		case Bytecode.OPCODE_aliasdecl:
			//return translateAliasDeclaration((Location<Bytecode.AliasDeclaration>) c);
		case Bytecode.OPCODE_assert:
			//return translateAssert((Location<Bytecode.Assert>) c);
		case Bytecode.OPCODE_assume:
			//return translateAssume((Location<Bytecode.Assume>) c);
		case Bytecode.OPCODE_assign:
			//return translateAssign((Location<Bytecode.Assign>) c);
		case Bytecode.OPCODE_break:
			//return translateBreak((Location<Bytecode.Break>) c);
		case Bytecode.OPCODE_continue:
			//return translateContinue((Location<Bytecode.Continue>) c);
		case Bytecode.OPCODE_debug:
			//return translateDebug((Location<Bytecode.Debug>) c);
		case Bytecode.OPCODE_dowhile:
			//return translateDoWhile((Location<Bytecode.DoWhile>) c);
		case Bytecode.OPCODE_fail:
			//return translateFail((Location<Bytecode.Fail>) c);
		case Bytecode.OPCODE_if:
		case Bytecode.OPCODE_ifelse:
			//return translateIf((Location<Bytecode.If>) c);
		case Bytecode.OPCODE_indirectinvoke:
			//return translateIndirectInvoke((Location<Bytecode.IndirectInvoke>) c);
		case Bytecode.OPCODE_invoke:
			//return translateInvoke((Location<Bytecode.Invoke>) c);
		case Bytecode.OPCODE_namedblock:
			//return translateNamedBlock((Location<Bytecode.NamedBlock>) c);
		case Bytecode.OPCODE_while:
			//return translateWhile((Location<Bytecode.While>) c);
		case Bytecode.OPCODE_return:
			return translateReturn((Location<Bytecode.Return>) c);
		case Bytecode.OPCODE_skip:
			//return translateSkip((Location<Bytecode.Skip>) c);
		case Bytecode.OPCODE_switch:
			//return translateSwitch((Location<Bytecode.Switch>) c);
		case Bytecode.OPCODE_vardecl:
		case Bytecode.OPCODE_vardeclinit:
			return translateVariableDeclaration((Location<Bytecode.VariableDeclaration>) c);
		default:
			throw new IllegalArgumentException("unknown bytecode encountered");
		}
	}

	private JavaFile.Term translateVariableDeclaration(Location<Bytecode.VariableDeclaration> stmt) {
		Bytecode.VariableDeclaration d = stmt.getBytecode();
		JavaFile.Type type = translateType(stmt.getType());
		JavaFile.Term initialiser = null;
		if(stmt.numberOfOperands() > 0) {
			initialiser = translateExpression(stmt.getOperand(0));
		}
		return new JavaFile.VariableDeclaration(type,d.getName(),initialiser);
	}

	private JavaFile.Term translateReturn(Location<Bytecode.Return> stmt) {
		JavaFile.Term initialiser = null;
		if(stmt.numberOfOperands() > 0) {
			initialiser = translateExpression(stmt.getOperand(0));
		}
		return new JavaFile.Return(initialiser);
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

	// @SuppressWarnings("unchecked")
	private JavaFile.Term translateExpression(Location<?> expr) {
		switch (expr.getOpcode()) {
		case Bytecode.OPCODE_arraylength:
		case Bytecode.OPCODE_arrayindex:
		case Bytecode.OPCODE_array:
		case Bytecode.OPCODE_arraygen:
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
			return translateRecordConstructor((Location<Bytecode.Operator>) expr);
		case Bytecode.OPCODE_all:
		case Bytecode.OPCODE_some:
			return translateQuantifier((Location<Bytecode.Quantifier>) expr);
		case Bytecode.OPCODE_varaccess:
			return translateVariableAccess((Location<Bytecode.VariableAccess>) expr);
		default:
			throw new IllegalArgumentException("unknown bytecode encountered: " +
					expr.getBytecode());
		}
	}

	private JavaFile.Term translateOperator(Location<Bytecode.Operator> expr) {
		List<JavaFile.Term> children = new ArrayList<>();
		for(int i=0;i!=expr.numberOfOperands();++i) {
			children.add(translateExpression(expr.getOperand(i)));
		}
		JavaFile.Operator.Kind kind = translateOpcode(expr.getBytecode().kind());
		return new JavaFile.Operator(kind,children);
	}

	private JavaFile.Term translateConvert(Location<Bytecode.Convert> expr) {
//		out.print("(");
//		writeType(expr.getType());
//		out.print(") ");
//		writeExpression(expr.getOperand(0));
		return null;
	}

	private JavaFile.Term translateConst(Location<Bytecode.Const> expr) {
		Constant c = expr.getBytecode().constant();
		Object value;
		if(c instanceof Constant.Null) {
			value = null;
		} else if(c instanceof Constant.Bool) {
			Constant.Bool bc = (Constant.Bool) c;
			value = bc.value();
		} else if(c instanceof Constant.Integer) {
			Constant.Integer bc = (Constant.Integer) c;
			// FIXME: bug for large integer values here
			BigInteger bi = bc.value();
			long lv = bi.longValue();
			if(lv >= Integer.MIN_VALUE && lv < Integer.MAX_VALUE) {
				value = (int) lv;
			} else  {
				value = lv;
			}
		} else {
			throw new IllegalArgumentException("GOT HERE");
		}
		return new JavaFile.Constant(value);
	}

	private JavaFile.Term translateFieldLoad(Location<Bytecode.FieldLoad> expr) {
//		writeBracketedExpression(expr.getOperand(0));
//		out.print("." + expr.getBytecode().fieldName());
		return null;
	}

	private JavaFile.Term translateIndirectInvoke(Location<Bytecode.IndirectInvoke> expr)
	{
//		Location<?>[] operands = expr.getOperands();
//		writeExpression(operands[0]);
//		out.print("(");
//		for (int i = 1; i != operands.length; ++i) {
//			if (i != 1) {
//				out.print(", ");
//			}
//			writeExpression(operands[i]);
//		}
//		out.print(")");
		return null;
	}

	private JavaFile.Term translateInvoke(Location<Bytecode.Invoke> expr) {
//		out.print(expr.getBytecode().name() + "(");
//		Location<?>[] operands = expr.getOperands();
//		for (int i = 0; i != operands.length; ++i) {
//			if (i != 0) {
//				out.print(", ");
//			}
//			writeExpression(operands[i]);
//		}
//		out.print(")");
		return null;
	}

	@SuppressWarnings("unchecked")
	private JavaFile.Term translateLambda(Location<Bytecode.Lambda> expr) {
//		out.print("&[");
//		Location<?>[] environment = expr.getOperandGroup(SyntaxTree.ENVIRONMENT);
//		for (int i = 0; i != environment.length; ++i) {
//			Location<VariableDeclaration> var = (Location<VariableDeclaration>) environment[i];
//			if (i != 0) {
//				out.print(", ");
//			}
//			out.print(var.getType());
//			out.print(" ");
//			out.print(var.getBytecode().getName());
//		}
//		out.print("](");
//		Location<?>[] parameters = expr.getOperandGroup(SyntaxTree.PARAMETERS);
//		for (int i = 0; i != parameters.length; ++i) {
//			Location<VariableDeclaration> var = (Location<VariableDeclaration>) parameters[i];
//			if (i != 0) {
//				out.print(", ");
//			}
//			out.print(var.getType());
//			out.print(" ");
//			out.print(var.getBytecode().getName());
//		}
//		out.print(" -> ");
//		writeExpression(expr.getOperand(0));
//		out.print(")");
		return null;
	}

	private JavaFile.Term translateRecordConstructor(Location<Bytecode.Operator> expr) {
//		Type.EffectiveRecord t = (Type.EffectiveRecord) expr.getType();
//		String[] fields = t.getFieldNames();
//		Location<?>[] operands = expr.getOperands();
//		out.print("{");
//		for (int i = 0; i != operands.length; ++i) {
//			if (i != 0) {
//				out.print(", ");
//			}
//			out.print(fields[i]);
//			out.print(" ");
//			writeExpression(operands[i]);
//		}
//		out.print("}");
		return null;
	}

	@SuppressWarnings("unchecked")
	private JavaFile.Term translateQuantifier(Location<Bytecode.Quantifier> c) {
//		out.print(quantifierKind(c));
//		out.print(" { ");
//		for (int i = 0; i != c.numberOfOperandGroups(); ++i) {
//			Location<?>[] range = c.getOperandGroup(i);
//			if (i != 0) {
//				out.print(", ");
//			}
//			Location<VariableDeclaration> v = (Location<VariableDeclaration>) range[SyntaxTree.VARIABLE];
//			out.print(v.getBytecode().getName());
//			out.print(" in ");
//			writeExpression(range[SyntaxTree.START]);
//			out.print("..");
//			writeExpression(range[SyntaxTree.END]);
//		}
//		out.print(" | ");
//		writeExpression(c.getOperand(SyntaxTree.CONDITION));
//		out.print(" } ");
		return null;
	}

	private JavaFile.Term translateVariableAccess(Location<Bytecode.VariableAccess> expr) {
		Location<VariableDeclaration> vd = getVariableDeclaration(expr.getOperand(0));
		JavaFile.Term t = new JavaFile.VariableAccess(vd.getBytecode().getName());
		JavaFile.Type type = translateType(expr.getType());
		if(!isCopyable(type)) {
			throw new RuntimeException("GOT HERE");
		}
		return t;
	}

	private static void addModifiers(JavaFile.Declaration d, List<Modifier> modifiers) {
		for (Modifier m : modifiers) {
			if (m == Modifier.PUBLIC) {
				d.getModifiers().add(JavaFile.Modifier.PUBLIC);
			} else if (m == Modifier.PRIVATE) {
				d.getModifiers().add(JavaFile.Modifier.PRIVATE);
			}
		}
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
		} else {
			// Default
			return new JavaFile.Reference("Object");
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
			put(Type.T_INT, new JavaFile.Reference("BigInteger"));
			put(Type.T_ANY, new JavaFile.Reference("Object"));
		}
	};

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
		if (type instanceof JavaFile.Primitive) {
			return true;
		} else {
			// FIXME: could do better here, e.g. for immutable reference types
			return false;
		}
	}

	private static JavaFile.Operator.Kind translateOpcode(Bytecode.OperatorKind k) {
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
