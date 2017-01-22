// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.
package wyjc.core;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

import wybs.lang.CompilationUnit;
import wybs.util.AbstractCompilationUnit;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wycc.util.Pair;
import wyjc.io.JavaFileWriter;

public class JavaFile extends AbstractCompilationUnit {
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
			// At this stage, parsing java files is strictly off-limits :)
			throw new UnsupportedOperationException();
		}

		@Override
		public void write(OutputStream output, JavaFile jf) throws IOException {
			new JavaFileWriter(output).write(jf);
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

	/**
	 * The list of top-level declarations within this file.
	 */
	private List<Declaration> declarations;

	public JavaFile(Path.Entry<? extends CompilationUnit> entry) {
		super(entry);
		this.declarations = new ArrayList<>();
	}

	public List<Declaration> getDeclarations() {
		return declarations;
	}

	/**
	 * A declaration (e.g. class or method) within a Java file
	 *
	 * @author David J. Pearce
	 *
	 */
	public interface Declaration {
		List<Modifier> getModifiers();
	}

	public enum Modifier {
		PUBLIC,
		PROTECTED,
		PRIVATE,
		FINAL,
		STATIC
	}

	private static class AbstractDeclaration implements Declaration {
		/**
		 * The name of the class in question
		 */
		private final String name;

		/**
		 * The modifiers associated with this class
		 */
		private final List<Modifier> modifiers = new ArrayList<>();

		public AbstractDeclaration(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		@Override
		public List<Modifier> getModifiers() {
			return modifiers;
		}
	}

	public static class Class extends AbstractDeclaration implements Declaration {


		/**
		 * The set of declarations within the class
		 */
		private final List<Declaration> declarations = new ArrayList<>();

		public Class(String name) {
			super(name);
		}

		public List<Declaration> getDeclarations() {
			return declarations;
		}
	}

	/**
	 * Represents a method declaration within a given Java class.
	 *
	 * @author David J. Pearce
	 *
	 */
	public static class Method extends AbstractDeclaration implements Declaration {
		private Type returnType;
		private List<Pair<Type,String>> parameters = new ArrayList<>();

		public Method(String name, Type returnType) {
			super(name);
			this.returnType = returnType;
		}

		public Type getReturnType() {
			return returnType;
		}

		public List<Pair<Type,String>> getParameters() {
			return parameters;
		}
	}

	/**
	 * Represents a type in a Java source. Such types do not have to be fully
	 * qualified, though they can be.
	 *
	 * @author David J. Pearce
	 *
	 */
	public interface Type {

	}

	/**
	 * A generic class for representing primitive types in Java (e.g. boolean,
	 * int, etc)
	 *
	 * @author David J. Pearce
	 *
	 */
	public static class Primitive implements Type {
		public enum Kind {
			VOID, BOOLEAN, BYTE, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE
		}
		private Kind kind;

		public Primitive(Kind kind) {
			this.kind = kind;
		}

		public Kind getKind() { return kind; }
	}

	public static final Primitive VOID = new Primitive(Primitive.Kind.VOID);
	public static final Primitive BOOLEAN = new Primitive(Primitive.Kind.BOOLEAN);
	public static final Primitive BYTE = new Primitive(Primitive.Kind.BYTE);
	public static final Primitive CHAR = new Primitive(Primitive.Kind.CHAR);
	public static final Primitive SHORT = new Primitive(Primitive.Kind.SHORT);
	public static final Primitive INT = new Primitive(Primitive.Kind.INT);
	public static final Primitive LONG = new Primitive(Primitive.Kind.LONG);

	public static class Array implements Type {
		private Type element;
		public Array(Type element) {
			this.element = element;
		}
		public Type getElement() {
			return element;
		}
	}

	public static class Reference implements Type {
		private String[] elements;
		// FIXME: missing generics
		public Reference(String... elements) {
			this.elements = elements;
		}
		public String[] getElements() {
			return elements;
		}
		public int size() {
			return elements.length;
		}
		public String get(int i) {
			return elements[i];
		}
	}
}
