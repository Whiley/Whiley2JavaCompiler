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
		out.println(") {");
		tab(indent);
		out.println("}");
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
