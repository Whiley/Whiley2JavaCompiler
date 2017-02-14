// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.io;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.List;

import wycc.util.Pair;
import wyjc.core.*;

public class JavaFileWriter {
	private final PrintWriter out;

	public JavaFileWriter(OutputStream output) {
		this.out = new PrintWriter(output);
	}

	public void write(JavaFile file) {
		writeImports();
		for(JavaFile.Declaration d : file.getDeclarations()) {
			write(0,(JavaFile.Declaration)d);
		}
		out.flush();
		out.close();
	}

	private void writeImports() {
		out.println("import java.math.BigInteger;");
	}

	private void write(int indent, JavaFile.Declaration d) {
		if(d instanceof JavaFile.Class) {
			write(indent,(JavaFile.Class)d);
 		} else if(d instanceof JavaFile.Method) {
			write(indent,(JavaFile.Method)d);
 		} else if(d instanceof JavaFile.Constructor) {
			write(indent,(JavaFile.Constructor)d);
 		} else if(d instanceof JavaFile.Field) {
			write(indent,(JavaFile.Field)d);
 		} else {
 			throw new RuntimeException("Unknown declaration encountered");
 		}
	}

	private void write(int indent, JavaFile.Class classDecl) {
		tab(indent);
		writeModifiers(classDecl.getModifiers());
		out.print("class ");
		out.print(classDecl.getName());
		out.println(" {");
		for(JavaFile.Declaration decl : classDecl.getDeclarations()) {
			write(indent+1,decl);
		}
		tab(indent);
		out.println("}");
	}

	private void write(int indent, JavaFile.Method method) {
		tab(indent);
		writeModifiers(method.getModifiers());
		writeType(method.getReturnType());
		out.print(" ");
		out.print(method.getName());
		out.print("(");
		List<Pair<JavaFile.Type,String>> parameters = method.getParameters();
		for(int i=0;i!=parameters.size();++i) {
			if(i != 0) {
				out.print(", ");
			}
			Pair<JavaFile.Type,String> p = parameters.get(i);
			writeType(p.first());
			out.print(" ");
			out.print(p.second());
		}
		out.print(")");
		if(method.getBody() != null) {
			writeBlock(indent,method.getBody());
			out.println();
		} else {
			out.println(";");
		}

	}

	private void write(int indent, JavaFile.Constructor constructor) {
		tab(indent);
		writeModifiers(constructor.getModifiers());
		out.print(constructor.getName());
		out.print("(");
		List<Pair<JavaFile.Type,String>> parameters = constructor.getParameters();
		for(int i=0;i!=parameters.size();++i) {
			if(i != 0) {
				out.print(", ");
			}
			Pair<JavaFile.Type,String> p = parameters.get(i);
			writeType(p.first());
			out.print(" ");
			out.print(p.second());
		}
		out.print(")");
		if(constructor.getBody() != null) {
			writeBlock(indent,constructor.getBody());
			out.println();
		} else {
			out.println(";");
		}

	}

	private void write(int indent, JavaFile.Field fieldDecl) {
		tab(indent);
		writeModifiers(fieldDecl.getModifiers());
		writeType(fieldDecl.getType());
		out.print(" ");
		out.print(fieldDecl.getName());
		out.println(";");
	}

	private void writeBlock(int indent, JavaFile.Block block) {
		out.println(" {");
		for(JavaFile.Term term : block.getTerms()) {
			writeStatement(indent+1,term);
		}
		tab(indent);out.print("}");
	}

	private void writeStatement(int indent, JavaFile.Term term) {
		tab(indent);
		if(term instanceof JavaFile.Assert) {
			writeAssert(indent,(JavaFile.Assert) term);
		} else if(term instanceof JavaFile.Assignment) {
			writeAssignment(indent,(JavaFile.Assignment) term);
		} else if(term instanceof JavaFile.Break) {
			writeBreak(indent,(JavaFile.Break) term);
		} else if(term instanceof JavaFile.Continue) {
			writeContinue(indent,(JavaFile.Continue) term);
		} else if(term instanceof JavaFile.If) {
			writeIf(indent,(JavaFile.If) term);
		} else if(term instanceof JavaFile.Return) {
			writeReturn(indent,(JavaFile.Return) term);
		} else if(term instanceof JavaFile.VariableDeclaration) {
			writeVariableDeclaration(indent,(JavaFile.VariableDeclaration) term);
		} else if(term instanceof JavaFile.While) {
			writeWhile(indent,(JavaFile.While) term);
		} else {
			throw new IllegalArgumentException("unknown statement: " + term);
		}
	}

	private void writeAssert(int indent, JavaFile.Assert term) {
		out.print("assert ");
		writeExpression(term.getOperand());
		out.println(";");
	}

	private void writeAssignment(int indent, JavaFile.Assignment term) {
		writeExpression(term.getLefthandSide());
		out.print(" = ");
		writeExpression(term.getRighthandSide());
		out.println(";");
	}

	private void writeBreak(int indent, JavaFile.Break term) {
		out.print("break;");
	}

	private void writeContinue(int indent, JavaFile.Continue term) {
		out.print("continue;");
	}

	private void writeIf(int indent, JavaFile.If term) {
		out.print("if(");
		writeExpression(term.getCondition());
		out.print(") ");
		writeBlock(indent, term.getTrueBranch());
		if (term.getFalseBranch() != null) {
			out.print(" else ");
			writeBlock(indent, term.getFalseBranch());
		}
		out.println();
	}

	private void writeReturn(int indent, JavaFile.Return term) {
		out.print("return");
		if(term.getInitialiser() != null) {
			out.print(" ");
			writeExpression(term.getInitialiser());
		}
		out.println(";");
	}

	private void writeVariableDeclaration(int indent, JavaFile.VariableDeclaration term) {
		writeType(term.getType());
		out.print(" ");
		out.print(term.getName());
		if(term.getInitialiser() != null) {
			out.print(" = ");
			writeExpression(term.getInitialiser());
		}
		out.println(";");
	}

	private void writeWhile(int indent, JavaFile.While term) {
		out.print("while(");
		writeExpression(term.getCondition());
		out.print(") ");
		writeBlock(indent, term.getBody());
		out.println();
	}

	private void writeExpressionWithBraces(JavaFile.Term term) {
		if(term instanceof JavaFile.Operator) {
			JavaFile.Operator op = (JavaFile.Operator) term;
			if(!isPrefix(op.getKind())) {
				out.print("(");
				writeExpression(term);
				out.print(")");
				return;
			}
		}
		writeExpression(term);
	}

	private void writeExpression(JavaFile.Term term) {
		if(term instanceof JavaFile.ArrayAccess) {
			writeArrayAccess((JavaFile.ArrayAccess) term);
		} else if(term instanceof JavaFile.Cast) {
			writeCast((JavaFile.Cast) term);
		} else if(term instanceof JavaFile.Constant) {
			writeConstant((JavaFile.Constant) term);
		} else if(term instanceof JavaFile.FieldAccess) {
			writeFieldAccess((JavaFile.FieldAccess) term);
		} else if(term instanceof JavaFile.InstanceOf) {
			writeInstanceOf((JavaFile.InstanceOf) term);
		} else if(term instanceof JavaFile.Invoke) {
			writeInvoke((JavaFile.Invoke) term);
		} else if(term instanceof JavaFile.New) {
			writeNew((JavaFile.New) term);
		} else if(term instanceof JavaFile.Operator) {
			writeOperator((JavaFile.Operator) term);
		} else if(term instanceof JavaFile.VariableAccess) {
			writeVariableAccess((JavaFile.VariableAccess) term);
		} else {
			throw new IllegalArgumentException("unknown term encountered: " + term);
		}
	}

	private void writeArrayAccess(JavaFile.ArrayAccess term) {
		writeExpression(term.getSource());
		out.print("[");
		writeExpression(term.getIndex());
		out.print("]");
	}

	private void writeCast(JavaFile.Cast term) {
		out.print("(");
		writeType(term.getType());
		out.print(") ");
		writeExpression(term.getSource());
	}

	private void writeConstant(JavaFile.Constant term) {
		Object value = term.getValue();
		if(value instanceof String) {
			out.print("\"");
			out.print(term.getValue());
			out.print("\"");
		} else if(value instanceof Long) {
			out.print(term.getValue());
			out.print("L");
		} else {
			out.print(term.getValue());
		}
	}

	private void writeFieldAccess(JavaFile.FieldAccess term) {
		writeExpression(term.getSource());
		out.print(".");
		out.print(term.getField());
	}

	private void writeInstanceOf(JavaFile.InstanceOf term) {
		writeExpression(term.getLefthandSide());
		out.print(" instanceof ");
		writeType(term.getRighthandSide());
	}

	private void writeInvoke(JavaFile.Invoke term) {
		JavaFile.Term receiver = term.getReceiver();
		if(receiver != null) {
			writeExpression(receiver);
			out.print(".");
		}
		writePath(term.getPath());
		out.print("(");
		writeArguments(term.getArguments());
		out.print(")");
	}

	private void writeOperator(JavaFile.Operator term) {
		JavaFile.Operator.Kind kind = term.getKind();
		String operator = getOperatorString(kind);
		List<JavaFile.Term> operands = term.getOperands();
		if(isPrefix(kind)) {
			out.print(operator);
			writeExpressionWithBraces(operands.get(0));
		} else {
			writeExpressionWithBraces(operands.get(0));
			out.print(" ");
			out.print(operator);
			out.print(" ");
			writeExpressionWithBraces(operands.get(1));
		}
	}

	private boolean isPrefix(JavaFile.Operator.Kind kind) {
		switch(kind) {
		case NOT:
		case NEG:
		case BITWISEINVERT:
			return true;
		default:
			return false;
		}
	}

	private String getOperatorString(JavaFile.Operator.Kind kind) {
		switch(kind) {
		case NOT:
			return "!";
		case NEG:
			return "-";
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
		case BITWISEINVERT:
			return "~";
		case LEFTSHIFT:
			return "<<";
		case RIGHTSHIFT:
			return ">>";
		default:
			throw new IllegalArgumentException("unknown operator kind: " + kind);
		}
	}

	private void writeVariableAccess(JavaFile.VariableAccess term) {
		out.print(term.getName());
	}

	private void writeNew(JavaFile.New term) {
		out.print("new ");
		writeType(term.getType());
		List<JavaFile.Term> initialisers = term.getInitialisers();
		if(initialisers != null) {
			out.print("{");
			writeArguments(initialisers);
			out.print("}");
		}
	}

	/**
	 * Print out a comma-separated list of argument expressions.
	 *
	 * @param arguments
	 */
	private void writeArguments(List<JavaFile.Term> arguments) {
		for(int i=0;i!=arguments.size();++i) {
			if(i != 0) {
				out.print(", ");
			}
			writeExpression(arguments.get(i));
		}
	}


	private void writeType(JavaFile.Type type) {
		if(type instanceof JavaFile.Primitive) {
			JavaFile.Primitive pt = (JavaFile.Primitive) type;
			out.print(pt.getKind().toString().toLowerCase());
		} else if(type instanceof JavaFile.Array){
			JavaFile.Array at = (JavaFile.Array) type;
			writeType(at.getElement());
			out.print("[]");
		} else {
			JavaFile.Reference rt = (JavaFile.Reference) type;
			for(int i=0;i!=rt.size();++i) {
				if(i != 0) {
					out.print(".");
				}
				out.print(rt.get(i));
			}
		}
	}

	private void writePath(List<String> path) {
		for(int i=0;i!=path.size();++i) {
			if(i != 0) {
				out.print(".");
			}
			out.print(path.get(i));
		}
	}

	private void writeModifiers(List<JavaFile.Modifier> modifiers) {
		for(int i=0;i!=modifiers.size();++i) {
			out.print(modifiers.get(i).toString().toLowerCase());
			out.print(" ");
		}
	}

	private void tab(int indent) {
		for (int i = 0; i != indent; ++i) {
			out.print("\t");
		}
	}
}
