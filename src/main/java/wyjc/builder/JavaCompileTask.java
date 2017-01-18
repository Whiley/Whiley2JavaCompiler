package wyjc.builder;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import wybs.lang.Build;
import wybs.lang.NameID;
import wybs.lang.Build.Graph;
import wybs.util.AbstractCompilationUnit;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.lang.Path.Entry;
import wyfs.lang.Path.Root;
import wyfs.util.Trie;
import wyil.lang.Bytecode;
import wyil.lang.Constant;
import wyil.lang.Modifier;
import wyil.lang.SyntaxTree;
import wyil.lang.Type;
import wyil.lang.WyilFile;
import wyil.lang.Bytecode.AliasDeclaration;
import wyil.lang.Bytecode.VariableAccess;
import wyil.lang.Bytecode.VariableDeclaration;
import wyil.lang.SyntaxTree.Location;
import wyil.lang.WyilFile.FunctionOrMethod;
import wyil.util.TypeSystem;

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
			JavaFile contents = build(source.read());

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

	private PrintWriter out;

	private JavaFile build(WyilFile file) throws IOException {
		ByteArrayOutputStream stream = new ByteArrayOutputStream();
		//this.out = new PrintWriter(new OutputStreamWriter(stream));
		this.out = new PrintWriter(System.out);
		write(file);
		out.flush();
		return null;
	}

	private void write(WyilFile file) {
		out.println("public class " + file.getEntry().id().last() + " implements Cloneable {");
		//
		for (WyilFile.Block b : file.blocks()) {
			if(b instanceof WyilFile.Declaration) {
				WyilFile.Declaration d = (WyilFile.Declaration) b;
				writeLocationsAsComments(d.getTree());
				if(b instanceof WyilFile.FunctionOrMethod) {
					write(1,(WyilFile.FunctionOrMethod) b);
				} else if(b instanceof WyilFile.Type) {
					write(1,(WyilFile.Type) b);
				}
			}
			out.println();
		}
		out.println("}");
	}

	private void write(int indent, WyilFile.Type decl) {
		Type type = decl.type();

		if(type instanceof Type.Record) {
			Type.Record recT = (Type.Record) type;
			tabIndent(indent);
			writeModifiers(decl.modifiers());
			out.println("static class " + decl.name() + " {");
			// Write field declartions
			writeFieldDeclarations(indent,recT);
			writeRecordConstructor(indent,decl.name(),recT);
			writeRecordEquals(indent,decl.name(),recT);
			writeRecordHashCode(indent,decl.name(),recT);
			writeRecordClone(indent,decl.name(),recT);
			tabIndent(indent);
			out.println("}");
		}
	}

	private void writeFieldDeclarations(int indent, Type.Record recT) {
		String[] fields = recT.getFieldNames();
		for(int i=0;i!=recT.size();++i) {
			String field = fields[i];
			tabIndent(indent+1);
			out.print("public ");
			writeType(recT.getField(field));
			out.println(" " + field + ";");
		}
		out.println();
	}

	private void writeRecordConstructor(int indent, String name, Type.Record recT) {
		tabIndent(indent+1);
		out.print("public ");
		out.print(name);
		out.print("(");
		String[] fields = recT.getFieldNames();
		for(int i=0;i!=recT.size();++i) {
			if(i != 0) {
				out.print(", ");
			}
			String field = fields[i];
			writeType(recT.getField(field));
			out.print(" " + field);
		}
		out.println(") {");
		for(int i=0;i!=recT.size();++i) {
			tabIndent(indent+2);
			String field = fields[i];
			out.println("this." + field + " = " + field + ";");
		}
		tabIndent(indent+1);
		out.println("}");
		out.println();
	}

	private void writeRecordEquals(int indent, String name, Type.Record recT) {
		tabIndent(indent+1);
		out.println("@Override");
		tabIndent(indent+1);
		out.println("public boolean equals(Object o) {");
		tabIndent(indent+2);
		out.println("if(o instanceof " + name + ") {");
		tabIndent(indent+3);
		out.println(name + " r = (" + name + ") o;");
		tabIndent(indent+3);
		out.print("return ");
		String[] fields = recT.getFieldNames();
		for(int i=0;i!=recT.size();++i) {
			if(i != 0) {
				out.print(" && ");
			}
			String field = fields[i];
			out.print(field + " == r." + field);
		}
		out.println(";");
		tabIndent(indent+2);
		out.println("}");
		tabIndent(indent+2);
		out.println("return false;");
		tabIndent(indent+1);
		out.println("}");
		out.println();
	}

	private void writeRecordHashCode(int indent, String name, Type.Record recT) {
		tabIndent(indent+1);
		out.println("@Override");
		tabIndent(indent+1);
		out.println("public int hashCode() {");
		tabIndent(indent+2);
		out.print("return ");
		String[] fields = recT.getFieldNames();
		for(int i=0;i!=recT.size();++i) {
			if(i != 0) {
				out.print(" ^ ");
			}
			String field = fields[i];
			out.print(field);
		}
		out.println(";");
		tabIndent(indent+1);
		out.println("}");
		out.println();
	}

	private void writeRecordClone(int indent, String name, Type.Record recT) {
		tabIndent(indent+1);
		out.println("@Override");
		tabIndent(indent+1);
		out.println("public Object clone() {");
		tabIndent(indent+2);
		out.print("return new " + name + "(");
		String[] fields = recT.getFieldNames();
		for(int i=0;i!=recT.size();++i) {
			if(i != 0) {
				out.print(",");
			}
			String field = fields[i];
			out.print(field);
		}
		out.println(");");
		tabIndent(indent+1);
		out.println("}");
	}

	private void write(int indent, FunctionOrMethod decl) {
		Type.FunctionOrMethod ft = decl.type();
		//
		tabIndent(indent);
		writeModifiers(decl.modifiers());
		out.print("static ");
		writeReturns(ft.returns());
		out.print(" ");
		out.print(decl.name());
		writeParameters(decl);
		//
		if (decl.getBody() != null) {
			out.println(" {");
			writeBlock(indent+1, decl.getBody());
			tabIndent(indent);out.println("}");
		} else {
			out.println();
		}
	}

	private void writeLocationsAsComments(SyntaxTree tree) {
		List<Location<?>> locations = tree.getLocations();
		for (int i = 0; i != locations.size(); ++i) {
			Location<?> loc = locations.get(i);
			String id = String.format("%1$" + 3 + "s", "#" + i);
			String type = String.format("%1$-" + 8 + "s", Arrays.toString(loc.getTypes()));
			out.println("// " + id + " " + type + " " + loc.getBytecode());
		}
	}

	private void writeReturns(Type[] returns) {
		if (returns.length > 1) {
			throw new RuntimeException("Missing support for multiple returns");
		} else if (returns.length == 0) {
			out.print("void");
		} else {
			writeType(returns[0]);
		}
	}

	private void writeParameters(WyilFile.FunctionOrMethod fm) {
		Type.FunctionOrMethod ft = fm.type();
		SyntaxTree tree = fm.getTree();
		Type[] parameters = ft.params();
		out.print("(");
		for (int i = 0; i != parameters.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			writeType(parameters[i]);
			out.print(" ");
			Location<VariableDeclaration> d = (Location<VariableDeclaration>) tree.getLocation(i);
			out.print(d.getBytecode().getName());
		}
		out.print(")");
	}

	private void writeBlock(int indent, Location<Bytecode.Block> block) {
		for (int i = 0; i != block.numberOfOperands(); ++i) {
			writeStatement(indent, block.getOperand(i));
		}
	}

	@SuppressWarnings("unchecked")
	private void writeStatement(int indent, Location<?> c) {
		tabIndent(indent);
		switch (c.getOpcode()) {
		case Bytecode.OPCODE_aliasdecl:
			writeAliasDeclaration(indent, (Location<Bytecode.AliasDeclaration>) c);
			break;
		case Bytecode.OPCODE_assert:
			writeAssert(indent, (Location<Bytecode.Assert>) c);
			break;
		case Bytecode.OPCODE_assume:
			writeAssume(indent, (Location<Bytecode.Assume>) c);
			break;
		case Bytecode.OPCODE_assign:
			writeAssign(indent, (Location<Bytecode.Assign>) c);
			break;
		case Bytecode.OPCODE_break:
			writeBreak(indent, (Location<Bytecode.Break>) c);
			break;
		case Bytecode.OPCODE_continue:
			writeContinue(indent, (Location<Bytecode.Continue>) c);
			break;
		case Bytecode.OPCODE_debug:
			writeDebug(indent, (Location<Bytecode.Debug>) c);
			break;
		case Bytecode.OPCODE_dowhile:
			writeDoWhile(indent, (Location<Bytecode.DoWhile>) c);
			break;
		case Bytecode.OPCODE_fail:
			writeFail(indent, (Location<Bytecode.Fail>) c);
			break;
		case Bytecode.OPCODE_if:
		case Bytecode.OPCODE_ifelse:
			writeIf(indent, (Location<Bytecode.If>) c);
			break;
		case Bytecode.OPCODE_indirectinvoke:
			writeIndirectInvoke(indent, (Location<Bytecode.IndirectInvoke>) c);
			break;
		case Bytecode.OPCODE_invoke:
			writeInvoke(indent, (Location<Bytecode.Invoke>) c);
			break;
		case Bytecode.OPCODE_namedblock:
			writeNamedBlock(indent, (Location<Bytecode.NamedBlock>) c);
			break;
		case Bytecode.OPCODE_while:
			writeWhile(indent, (Location<Bytecode.While>) c);
			break;
		case Bytecode.OPCODE_return:
			writeReturn(indent, (Location<Bytecode.Return>) c);
			break;
		case Bytecode.OPCODE_skip:
			writeSkip(indent, (Location<Bytecode.Skip>) c);
			break;
		case Bytecode.OPCODE_switch:
			writeSwitch(indent, (Location<Bytecode.Switch>) c);
			break;
		case Bytecode.OPCODE_vardecl:
		case Bytecode.OPCODE_vardeclinit:
			writeVariableDeclaration(indent, (Location<Bytecode.VariableDeclaration>) c);
			break;
		default:
			throw new IllegalArgumentException("unknown bytecode encountered");
		}
	}

	private void writeAliasDeclaration(int indent, Location<AliasDeclaration> loc) {
		out.print("alias ");
		out.print(loc.getType());
		out.print(" ");
		Location<VariableDeclaration> aliased = getVariableDeclaration(loc);
		out.print(aliased.getBytecode().getName());
		out.println(";");
	}

	private void writeAssert(int indent, Location<Bytecode.Assert> c) {
		out.print("assert ");
		writeExpression(c.getOperand(0));
		out.println(";");
	}

	private void writeAssume(int indent, Location<Bytecode.Assume> c) {
		out.print("assume ");
		writeExpression(c.getOperand(0));
		out.println(";");
	}

	private void writeAssign(int indent, Location<Bytecode.Assign> stmt) {
		Location<?>[] lhs = stmt.getOperandGroup(SyntaxTree.LEFTHANDSIDE);
		Location<?>[] rhs = stmt.getOperandGroup(SyntaxTree.RIGHTHANDSIDE);
		if (lhs.length > 0) {
			for (int i = 0; i != lhs.length; ++i) {
				if (i != 0) {
					out.print(", ");
				}
				writeExpression(lhs[i]);
			}
			out.print(" = ");
		}
		writeExpressions(rhs);
		out.println(";");
	}

	private void writeBreak(int indent, Location<Bytecode.Break> b) {
		out.println("break;");
	}

	private void writeContinue(int indent, Location<Bytecode.Continue> b) {
		out.println("continue;");
	}

	private void writeDebug(int indent, Location<Bytecode.Debug> b) {

	}

	private void writeDoWhile(int indent, Location<Bytecode.DoWhile> b) {
		Location<?>[] loopInvariant = b.getOperandGroup(0);
		Location<?>[] modifiedOperands = b.getOperandGroup(1);
		out.println("do:");
		//
		writeBlock(indent + 1, b.getBlock(0));
		tabIndent(indent);
		out.print("while ");
		writeExpression(b.getOperand(0));
		out.print(" modifies ");
		writeExpressions(modifiedOperands);
		for (Location<?> invariant : loopInvariant) {
			out.println();
			tabIndent(indent + 1);
			out.print("where ");
			writeExpression(invariant);
		}
		// FIXME: add invariants
		out.println();
	}

	private void writeFail(int indent, Location<Bytecode.Fail> c) {

	}

	private void writeIf(int indent, Location<Bytecode.If> b) {
		out.print("if(");
		writeExpression(b.getOperand(0));
		out.println(") {");
		writeBlock(indent + 1, b.getBlock(0));
		//
		if (b.numberOfBlocks() > 1) {
			tabIndent(indent);
			out.println("} else {");
			writeBlock(indent + 1, b.getBlock(1));
		}
		//
		tabIndent(indent);
		out.println("}");
	}

	private void writeIndirectInvoke(int indent, Location<Bytecode.IndirectInvoke> stmt) {
		Location<?>[] operands = stmt.getOperands();
		writeExpression(operands[0]);
		out.print("(");
		for (int i = 1; i != operands.length; ++i) {
			if (i != 1) {
				out.print(", ");
			}
			writeExpression(operands[i]);
		}
		out.println(")");
	}

	private void writeInvoke(int indent, Location<Bytecode.Invoke> stmt) {
		out.print(stmt.getBytecode().name() + "(");
		Location<?>[] operands = stmt.getOperands();
		for (int i = 0; i != operands.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			writeExpression(operands[i]);
		}
		out.println(")");
	}

	private void writeNamedBlock(int indent, Location<Bytecode.NamedBlock> b) {
		out.print(b.getBytecode().getName());
		out.println(": {");
		writeBlock(indent + 1, b.getBlock(0));
		tabIndent(indent);out.println("}");
	}

	private void writeWhile(int indent, Location<Bytecode.While> b) {
		out.print("while(");
		writeExpression(b.getOperand(0));
		out.println(") {");
		writeBlock(indent + 1, b.getBlock(0));
		tabIndent(indent);
		out.println("}");
	}

	private void writeReturn(int indent, Location<Bytecode.Return> b) {
		Location<?>[] operands = b.getOperands();
		out.print("return");
		if (operands.length > 0) {
			out.print(" ");
			writeExpressions(operands);
		}
		out.println(";");
	}

	private void writeSkip(int indent, Location<Bytecode.Skip> b) {
		out.println("// skip");
	}

	private void writeSwitch(int indent, Location<Bytecode.Switch> b) {
		out.print("switch ");
		writeExpression(b.getOperand(0));
		out.println(":");
		for (int i = 0; i != b.numberOfBlocks(); ++i) {
			// FIXME: ugly
			Bytecode.Case cAse = b.getBytecode().cases()[i];
			Constant[] values = cAse.values();
			tabIndent(indent + 2);
			if (values.length == 0) {
				out.println("default:");
			} else {
				out.print("case ");
				for (int j = 0; j != values.length; ++j) {
					if (j != 0) {
						out.print(", ");
					}
					out.print(values[j]);
				}
				out.println(":");
			}
			writeBlock(indent + 2, b.getBlock(i));
		}
	}

	private void writeVariableAccess(Location<VariableAccess> loc) {
		Location<VariableDeclaration> vd = getVariableDeclaration(loc.getOperand(0));
		out.print(vd.getBytecode().getName());
	}

	private void writeVariableDeclaration(int indent, Location<VariableDeclaration> loc) {
		Location<?>[] operands = loc.getOperands();
		writeType(loc.getType());
		out.print(" ");
		out.print(loc.getBytecode().getName());
		if (operands.length > 0) {
			out.print(" = ");
			writeExpression(operands[0]);
		}
		out.println(";");
	}

	/**
	 * Write a bracketed operand if necessary. Any operand whose human-readable
	 * representation can contain whitespace must have brackets around it.
	 *
	 * @param operand
	 * @param enclosing
	 * @param out
	 */
	private void writeBracketedExpression(Location<?> expr) {
		boolean needsBrackets = needsBrackets(expr.getBytecode());
		if (needsBrackets) {
			out.print("(");
		}
		writeExpression(expr);
		if (needsBrackets) {
			out.print(")");
		}
	}

	private void writeExpressions(Location<?>[] exprs) {
		for (int i = 0; i != exprs.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			writeExpression(exprs[i]);
		}
	}

	@SuppressWarnings("unchecked")
	private void writeExpression(Location<?> expr) {
		switch (expr.getOpcode()) {
		case Bytecode.OPCODE_arraylength:
			writeArrayLength((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_arrayindex:
			writeArrayIndex((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_array:
			writeArrayInitialiser((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_arraygen:
			writeArrayGenerator((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_convert:
			writeConvert((Location<Bytecode.Convert>) expr);
			break;
		case Bytecode.OPCODE_const:
			writeConst((Location<Bytecode.Const>) expr);
			break;
		case Bytecode.OPCODE_fieldload:
			writeFieldLoad((Location<Bytecode.FieldLoad>) expr);
			break;
		case Bytecode.OPCODE_indirectinvoke:
			writeIndirectInvoke((Location<Bytecode.IndirectInvoke>) expr);
			break;
		case Bytecode.OPCODE_invoke:
			writeInvoke((Location<Bytecode.Invoke>) expr);
			break;
		case Bytecode.OPCODE_lambda:
			writeLambda((Location<Bytecode.Lambda>) expr);
			break;
		case Bytecode.OPCODE_record:
			writeRecordConstructor((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_newobject:
			writeNewObject((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_dereference:
		case Bytecode.OPCODE_logicalnot:
		case Bytecode.OPCODE_neg:
		case Bytecode.OPCODE_bitwiseinvert:
			writePrefixLocations((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_all:
		case Bytecode.OPCODE_some:
			writeQuantifier((Location<Bytecode.Quantifier>) expr);
			break;
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
			writeInfixLocations((Location<Bytecode.Operator>) expr);
			break;
		case Bytecode.OPCODE_varaccess:
			writeVariableAccess((Location<VariableAccess>) expr);
			break;
		default:
			throw new IllegalArgumentException("unknown bytecode encountered: " + expr.getBytecode());
		}
	}

	private void writeArrayLength(Location<Bytecode.Operator> expr) {
		writeExpression(expr.getOperand(0));
		out.print(".length");
	}

	private void writeArrayIndex(Location<Bytecode.Operator> expr) {
		writeExpression(expr.getOperand(0));
		out.print("[");
		writeExpression(expr.getOperand(1));
		out.print("]");
	}

	private void writeArrayInitialiser(Location<Bytecode.Operator> expr) {
		Location<?>[] operands = expr.getOperands();
		out.print("new ");
		writeType(expr.getType());
		out.print("{");
		for (int i = 0; i != operands.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			writeExpression(operands[i]);
		}
		out.print("}");
	}

	private void writeArrayGenerator(Location<Bytecode.Operator> expr) {
		out.print("[");
		writeExpression(expr.getOperand(0));
		out.print(" ; ");
		writeExpression(expr.getOperand(1));
		out.print("]");
	}

	private void writeConvert(Location<Bytecode.Convert> expr) {
		out.print("(");
		writeType(expr.getType());
		out.print(") ");
		writeExpression(expr.getOperand(0));
	}

	private void writeConst(Location<Bytecode.Const> expr) {
		out.print(expr.getBytecode().constant());
	}

	private void writeFieldLoad(Location<Bytecode.FieldLoad> expr) {
		writeBracketedExpression(expr.getOperand(0));
		out.print("." + expr.getBytecode().fieldName());
	}

	private void writeIndirectInvoke(Location<Bytecode.IndirectInvoke> expr) {
		Location<?>[] operands = expr.getOperands();
		writeExpression(operands[0]);
		out.print("(");
		for (int i = 1; i != operands.length; ++i) {
			if (i != 1) {
				out.print(", ");
			}
			writeExpression(operands[i]);
		}
		out.print(")");
	}

	private void writeInvoke(Location<Bytecode.Invoke> expr) {
		out.print(expr.getBytecode().name() + "(");
		Location<?>[] operands = expr.getOperands();
		for (int i = 0; i != operands.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			writeExpression(operands[i]);
		}
		out.print(")");
	}

	@SuppressWarnings("unchecked")
	private void writeLambda(Location<Bytecode.Lambda> expr) {
		out.print("&[");
		Location<?>[] environment = expr.getOperandGroup(SyntaxTree.ENVIRONMENT);
		for (int i = 0; i != environment.length; ++i) {
			Location<VariableDeclaration> var = (Location<VariableDeclaration>) environment[i];
			if (i != 0) {
				out.print(", ");
			}
			out.print(var.getType());
			out.print(" ");
			out.print(var.getBytecode().getName());
		}
		out.print("](");
		Location<?>[] parameters = expr.getOperandGroup(SyntaxTree.PARAMETERS);
		for (int i = 0; i != parameters.length; ++i) {
			Location<VariableDeclaration> var = (Location<VariableDeclaration>) parameters[i];
			if (i != 0) {
				out.print(", ");
			}
			out.print(var.getType());
			out.print(" ");
			out.print(var.getBytecode().getName());
		}
		out.print(" -> ");
		writeExpression(expr.getOperand(0));
		out.print(")");
	}

	private void writeRecordConstructor(Location<Bytecode.Operator> expr) {
		Type.EffectiveRecord t = (Type.EffectiveRecord) expr.getType();
		String[] fields = t.getFieldNames();
		Location<?>[] operands = expr.getOperands();
		out.print("{");
		for (int i = 0; i != operands.length; ++i) {
			if (i != 0) {
				out.print(", ");
			}
			out.print(fields[i]);
			out.print(" ");
			writeExpression(operands[i]);
		}
		out.print("}");
	}

	private void writeNewObject(Location<Bytecode.Operator> expr) {
		out.print("new ");
		writeExpression(expr.getOperand(0));
	}

	private void writePrefixLocations(Location<Bytecode.Operator> expr) {
		// Prefix operators
		out.print(opcode(expr.getBytecode().kind()));
		writeBracketedExpression(expr.getOperand(0));
	}

	private void writeInfixLocations(Location<Bytecode.Operator> c) {
		writeBracketedExpression(c.getOperand(0));
		out.print(" ");
		out.print(opcode(c.getBytecode().kind()));
		out.print(" ");
		writeBracketedExpression(c.getOperand(1));

	}

	@SuppressWarnings("unchecked")
	private void writeQuantifier(Location<Bytecode.Quantifier> c) {
		out.print(quantifierKind(c));
		out.print(" { ");
		for (int i = 0; i != c.numberOfOperandGroups(); ++i) {
			Location<?>[] range = c.getOperandGroup(i);
			if (i != 0) {
				out.print(", ");
			}
			Location<VariableDeclaration> v = (Location<VariableDeclaration>) range[SyntaxTree.VARIABLE];
			out.print(v.getBytecode().getName());
			out.print(" in ");
			writeExpression(range[SyntaxTree.START]);
			out.print("..");
			writeExpression(range[SyntaxTree.END]);
		}
		out.print(" | ");
		writeExpression(c.getOperand(SyntaxTree.CONDITION));
		out.print(" } ");
	}

	private String quantifierKind(Location<Bytecode.Quantifier> c) {
		switch (c.getOpcode()) {
		case Bytecode.OPCODE_some:
			return "some";
		case Bytecode.OPCODE_all:
			return "all";
		}
		throw new IllegalArgumentException();
	}

	private boolean needsBrackets(Bytecode e) {
		switch (e.getOpcode()) {
		case Bytecode.OPCODE_convert:
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
		case Bytecode.OPCODE_newobject:
		case Bytecode.OPCODE_dereference:
			return true;
		}
		return false;
	}

	public void writeModifiers(List<Modifier> modifiers) {
		for(int i=0;i!=modifiers.size();++i) {
			out.print(modifiers.get(i));
			out.print(" ");
		}
	}

	public void writeType(Type type) {
		String cType = typeMap.get(type);
		if (cType != null) {
			out.print(cType);
		} else if(type instanceof Type.Nominal) {
			try {
				Type.Nominal tn = (Type.Nominal) type;
				NameID nid = tn.name();
				WyilFile f = project.get(nid.module(), WyilFile.ContentType).read();
				WyilFile.Type decl = f.type(nid.name());
				if(decl.type() instanceof Type.Record) {
					out.print(nid.name());
				} else {
					writeType(decl.type());
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		} else if(type instanceof Type.Union || type instanceof Type.Intersection || type instanceof Type.Negation) {
			out.print("Object");
		} else if (type instanceof Type.Array) {
			Type.Array arrT = (Type.Array) type;
			writeType(arrT.element());
			out.print("[]");
		} else {
			throw new IllegalArgumentException("Type not supported: " + type);
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

	private static HashMap<Type, String> typeMap=new HashMap<Type,String>(){{
		// Integer types
		put(TYPE_I8,"byte");
		put(TYPE_I16,"short");
		put(TYPE_I32,"int");
		put(TYPE_I64,"long");
		put(TYPE_U8,"short");
		put(TYPE_U16,"int");
		put(TYPE_U32,"long");
		//
		put(Type.T_BOOL,"boolean");
		put(Type.T_BYTE,"byte");
		put(Type.T_INT,"BigInteger");
		put(Type.T_ANY,"Object");
	}};

	private static String opcode(Bytecode.OperatorKind k) {
		switch (k) {
		case NEG:
			return "-";
		case NOT:
			return "!";
		case BITWISEINVERT:
			return "~";
		case DEREFERENCE:
			return "*";
		// Binary
		case ADD:
			return "+";
		case SUB:
			return "-";
		case MUL:
			return "*";
		case DIV:
			return "/";
		case REM:
			return "%";
		case EQ:
			return "==";
		case NEQ:
			return "!=";
		case LT:
			return "<";
		case LTEQ:
			return "<=";
		case GT:
			return ">";
		case GTEQ:
			return ">=";
		case AND:
			return "&&";
		case OR:
			return "||";
		case BITWISEOR:
			return "|";
		case BITWISEXOR:
			return "^";
		case BITWISEAND:
			return "&";
		case LEFTSHIFT:
			return "<<";
		case RIGHTSHIFT:
			return ">>";
		case IS:
			return "is";
		case NEW:
			return "new";
		default:
			throw new IllegalArgumentException("unknown operator kind : " + k);
		}
	}

	private void tabIndent(int indent) {
		indent = indent * 4;
		for (int i = 0; i < indent; ++i) {
			out.print(" ");
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
	// ========================================================================
	// JavaFile
	// ========================================================================
	private static class JavaFile extends AbstractCompilationUnit {
		// =========================================================================
		// Content Type
		// =========================================================================

		/**
		 * Responsible for identifying and reading/writing WyilFiles. The normal
		 * extension is ".wyil" for WyilFiles.
		 */
		public static final Content.Type<JavaFile> ContentType = new Content.Type<JavaFile>() {
			public Path.Entry<JavaFile> accept(Path.Entry<?> e) {
				if (e.contentType() == this) {
					return (Path.Entry<JavaFile>) e;
				}
				return null;
			}

			@Override
			public JavaFile read(Path.Entry<JavaFile> e, InputStream input) throws IOException {
				throw new RuntimeException("IMPLEMENT ME");
			}

			@Override
			public void write(OutputStream output, JavaFile module) throws IOException {
				throw new RuntimeException("IMPLEMENT ME");
			}

			@Override
			public String toString() {
				return "Content-Type: java";
			}

			@Override
			public String getSuffix() {
				return "java";
			}
		};

		public JavaFile(Entry entry) {
			super(entry);
		}
	}
}
