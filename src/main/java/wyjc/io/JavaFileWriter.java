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
import wyrl.core.Type.Term;

public class JavaFileWriter {
	private final PrintWriter out;

	public JavaFileWriter(OutputStream output) {
		this.out = new PrintWriter(output);
	}

	public void write(JavaFile file) {
		for(JavaFile.Declaration d : file.getDeclarations()) {
			write(0,(JavaFile.Declaration)d);
		}
		out.flush();
		out.close();
	}

	private void write(int indent, JavaFile.Declaration d) {
		if(d instanceof JavaFile.Class) {
			write(indent,(JavaFile.Class)d);
 		} else if(d instanceof JavaFile.Method) {
			write(indent,(JavaFile.Method)d);
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

	private void write(int indent, JavaFile.Method methodDecl) {
		tab(indent);
		writeModifiers(methodDecl.getModifiers());
		writeType(methodDecl.getReturnType());
		out.print(" ");
		out.print(methodDecl.getName());
		out.print("(");
		List<Pair<JavaFile.Type,String>> parameters = methodDecl.getParameters();
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
		if(methodDecl.getBody() != null) {
			writeBlock(indent,methodDecl.getBody());
		} else {
			out.println(";");
		}

	}

	private void writeBlock(int indent, JavaFile.Block block) {
		out.println(" {");
		for(JavaFile.Term term : block.getTerms()) {
			writeStatement(indent+1,term);
		}
		tab(indent);out.println("}");
	}

	private void writeStatement(int indent, JavaFile.Term term) {
		tab(indent);
		if(term instanceof JavaFile.Return) {
			writeReturn(indent,(JavaFile.Return) term);
		} else if(term instanceof JavaFile.VariableDeclaration) {
			writeVariableDeclaration(indent,(JavaFile.VariableDeclaration) term);
		} else {
			throw new IllegalArgumentException("unknown statement: " + term);
		}
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
		if(term instanceof JavaFile.Constant) {
			writeConstant((JavaFile.Constant) term);
		} else if(term instanceof JavaFile.Operator) {
			writeOperator((JavaFile.Operator) term);
		} else if(term instanceof JavaFile.VariableAccess) {
			writeVariableAccess((JavaFile.VariableAccess) term);
		} else {
			throw new IllegalArgumentException("unknown term encountered: " + term);
		}
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
