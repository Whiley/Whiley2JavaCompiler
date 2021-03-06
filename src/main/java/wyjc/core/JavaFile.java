// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.
package wyjc.core;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import wybs.lang.CompilationUnit;
import wybs.util.AbstractCompilationUnit;
import wyfs.lang.Content;
import wyfs.lang.Path;
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
	public interface Declaration extends Term {
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
	public static class Import implements Declaration {
		private final List<String> components;

		public Import(List<String> components) {
			this.components = new ArrayList<>(components);
		}

		public Import(String... components) {
			this.components = Arrays.asList(components);
		}

		public List<String> getComponents() {
			return components;
		}

		@Override
		public List<Modifier> getModifiers() {
			return Collections.EMPTY_LIST;
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
		private List<VariableDeclaration> parameters = new ArrayList<>();
		private Block body;

		public Method(String name, Type returnType) {
			super(name);
			this.returnType = returnType;
		}

		public Type getReturnType() {
			return returnType;
		}

		public List<VariableDeclaration> getParameters() {
			return parameters;
		}

		public Block getBody() {
			return body;
		}

		public void setBody(Block block) {
			this.body = block;
		}
	}

	public static class Constructor extends AbstractDeclaration implements Declaration {
		private List<VariableDeclaration> parameters = new ArrayList<>();
		private Block body;

		public Constructor(String name) {
			super(name);
		}

		public List<VariableDeclaration> getParameters() {
			return parameters;
		}

		public Block getBody() {
			return body;
		}

		public void setBody(Block block) {
			this.body = block;
		}
	}

	public static class Field extends VariableDeclaration implements Declaration {

		public Field(Type type, String name) {
			super(type,name);
		}

		public Field(Type type, String name, Term initialiser) {
			super(type,name,initialiser);
		}
	}

	public static class VariableDeclaration extends AbstractDeclaration implements Term {
		private Type type;
		private Term initialiser;

		public VariableDeclaration(Type type, String name) {
			super(name);
			this.type = type;
		}

		public VariableDeclaration(Type type, String name, Term initialiser) {
			super(name);
			this.type = type;
			this.initialiser = initialiser;
		}

		public Type getType() {
			return type;
		}

		public boolean hasInitialisr() {
			return initialiser != null;
		}

		public Term getInitialiser() {
			return initialiser;
		}
	}


	/**
	 * Represents either a statement or expression in a Java source file.
	 *
	 * @author David J. Pearce
	 *
	 */
	public interface Term {

	}

	public static class Block implements Term {
		private List<Term> terms;

		public Block() {
			terms = new ArrayList<>();
		}

		public Block(List<Term> terms) {
			this.terms = new ArrayList<>(terms);
		}

		public List<Term> getTerms() {
			return terms;
		}
	}

	public static class ArrayAccess implements Term {
		private Term src;
		private Term index;

		public ArrayAccess(Term src, Term index) {
			this.src = src;
			this.index = index;
		}

		public Term getSource() {
			return src;
		}

		public Term getIndex() {
			return index;
		}
	}

	public static class Assert implements Term {
		private Term operand;

		public Assert(Term operand) {
			this.operand = operand;
		}

		public Term getOperand() {
			return operand;
		}
	}

	public static class Assignment implements Term {
		private Term lhs;
		private Term rhs;

		public Assignment(Term lhs, Term rhs) {
			this.lhs = lhs;
			this.rhs = rhs;
		}

		public Term getLefthandSide() {
			return lhs;
		}

		public Term getRighthandSide() {
			return rhs;
		}
	}

	public static class Break implements Term {
	}

	public static class Cast implements Term {
		private Type type;
		private Term source;

		public Cast(Type type, Term source) {
			this.type = type;
			this.source = source;
		}

		public Type getType() {
			return type;
		}

		public Term getSource() {
			return source;
		}
	}

	public static class Continue implements Term {
	}

	public static class Constant implements Term {
		private Object value;

		public Constant(Object value) {
			if (value == null || value instanceof Boolean || value instanceof Byte || value instanceof Integer
					|| value instanceof Long || value instanceof String) {
				this.value = value;
			} else {
				throw new IllegalArgumentException("invalid constant value: " + value);
			}
		}

		public Object getValue() {
			return value;
		}
	}


	public static class DoWhile implements Term {
		private Block body;
		private Term condition;

		public DoWhile(Block body, Term condition) {
			this.condition = condition;
			this.body = body;
		}

		public Term getCondition() {
			return condition;
		}

		public Block getBody() {
			return body;
		}
	}

	public static class For implements Term {
		private VariableDeclaration initialiser;
		private Term condition;
		private Term increment;
		private Block body;

		public For(VariableDeclaration initialiser, Term condition, Term increment, Block body) {
			this.initialiser = initialiser;
			this.condition = condition;
			this.increment = increment;
			this.body = body;
		}

		public VariableDeclaration getInitialiser() {
			return initialiser;
		}

		public Term getCondition() {
			return condition;
		}

		public Term getIncrement() {
			return increment;
		}

		public Block getBody() {
			return body;
		}
	}

	public static class FieldAccess implements Term {
		private Term src;
		private String field;

		public FieldAccess(Term src, String field) {
			this.src = src;
			this.field = field;
		}

		public Term getSource() {
			return src;
		}

		public String getField() {
			return field;
		}
	}

	public static class If implements Term {
		private Term condition;
		private Block trueBranch;
		private Block falseBranch;

		public If(Term condition, Block trueBranch, Block falseBranch) {
			this.condition = condition;
			this.trueBranch = trueBranch;
			this.falseBranch = falseBranch;
		}

		public Term getCondition() {
			return condition;
		}

		public Block getTrueBranch() {
			return trueBranch;
		}

		public Block getFalseBranch() {
			return falseBranch;
		}
	}

	public static class IfElse implements Term {
		private List<Case> cases;

		public IfElse(List<Case> cases) {
			this.cases = cases;
		}

		public List<Case> getCases() {
			return cases;
		}

		public static class Case implements Term {
			private Term condition;
			private Block body;

			public Case(Term label, Block body) {
				this.condition = label;
				this.body = body;
			}

			public Term getLabel() {
				return condition;
			}

			public Block getBlock() {
				return body;
			}
		}
	}

	public static class InstanceOf implements Term {
		private Term lhs;
		private Type rhs;

		public InstanceOf(Term lhs, Type rhs) {
			this.lhs = lhs;
			this.rhs = rhs;
		}

		public Term getLefthandSide() {
			return lhs;
		}

		public Type getRighthandSide() {
			return rhs;
		}
	}

	public static class Invoke implements Term {
		private List<Type> typeArguments;
		private Term receiver;
		private List<String> path;
		private List<Term> arguments;

		public Invoke(Term receiver, String name, Term... arguments) {
			this.receiver = receiver;
			this.path = new ArrayList<>();
			this.path.add(name);
			this.arguments = Arrays.asList(arguments);
		}

		public Invoke(Type[] typeArgs, Term receiver, String name, Term... arguments) {
			this.typeArguments = Arrays.asList(typeArgs);
			this.receiver = receiver;
			this.path = new ArrayList<>();
			this.path.add(name);
			this.arguments = Arrays.asList(arguments);
		}

		public Invoke(Term receiver, String[] path, Term... arguments) {
			this.receiver = receiver;
			this.path = Arrays.asList(path);
			this.arguments = Arrays.asList(arguments);
		}

		public Invoke(Term receiver, String name, List<Term> arguments) {
			this.receiver = receiver;
			this.path = new ArrayList<>();
			this.path.add(name);
			this.arguments = new ArrayList<>(arguments);
		}


		public Invoke(Term receiver, List<String> path, List<Term> arguments) {
			this.receiver = receiver;
			this.path = new ArrayList<>(path);
			this.arguments = new ArrayList<>(arguments);
		}

		public List<Type> getTypeArguments() {
			return typeArguments;
		}

		public Term getReceiver() {
			return receiver;
		}

		public List<String> getPath() {
			return path;
		}

		public List<Term> getArguments() {
			return arguments;
		}
	}

	public static class Lambda implements Term {
		private List<VariableDeclaration> parameters;
		private Term body;

		public Lambda(List<VariableDeclaration> parameters, Term body) {
			this.parameters = parameters;
			this.body = body;
		}

		public List<VariableDeclaration> getParameters() {
			return parameters;
		}

		public Term getBody() {
			return body;
		}
	}

	public static class NewArray implements Term {
		private Array type;
		private Term size;
		private List<Term> initialisers;

		public NewArray(Array type, Term size, List<Term> initialisers) {
			this.type = type;
			this.size = size;
			this.initialisers = new ArrayList<>(initialisers);
		}

		public Array getType() {
			return type;
		}

		public Term getSizeOperand() {
			return size;
		}

		public List<Term> getInitialisers() {
			return initialisers;
		}
	}

	public static class New implements Term {
		private Reference type;
		private List<Term> parameters;

		public New(Reference type, List<Term> parameters) {
			this.type = type;
			this.parameters = new ArrayList<>(parameters);
		}

		public Type getType() {
			return type;
		}

		public List<Term> getParameters() {
			return parameters;
		}
	}

	public static class Operator implements Term {
		public enum Kind {
			NOT, NEG, EQ, NEQ, LT, LTEQ, GT, GTEQ, ADD, SUB, MUL, DIV, REM, AND, OR, BITWISEOR, BITWISEXOR, BITWISEAND, BITWISEINVERT, LEFTSHIFT, RIGHTSHIFT
		}
		private Kind kind;
		private List<Term> operands;

		public Operator(Kind kind, List<Term> operands) {
			this.kind = kind;
			this.operands = operands;
		}

		public Operator(Kind kind, Term... operands) {
			this.kind = kind;
			this.operands = Arrays.asList(operands);
		}

		public Kind getKind() {
			return kind;
		}

		public List<Term> getOperands() {
			return operands;
		}
	}

	public static class Return implements Term {
		private Term initialiser;

		public Return(Term initialiser) {
			this.initialiser = initialiser;
		}

		public Term getInitialiser() {
			return initialiser;
		}
	}

	public static class Case implements Term {
		private Constant label;
		private Term body;

		public Case(Constant label, Term body) {
			this.label = label;
			this.body = body;
		}

		public Constant getLabel() {
			return label;
		}

		public Term getBody() {
			return body;
		}
	}

	public static class Switch implements Term {
		private Term condition;
		private List<Case> cases;

		public Switch(Term condition, List<Case> cases) {
			this.condition = condition;
			this.cases = cases;
		}

		public Term getCondition() {
			return condition;
		}

		public List<Case> getCases() {
			return cases;
		}
	}

	public static class Throw implements Term {
		private JavaFile.Term term;

		public Throw(JavaFile.Term term) {
			this.term = term;
		}

		public Term getClause() {
			return term;
		}
	}

	public static class VariableAccess implements Term {
		private String name;

		public VariableAccess(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}
	}

	public static class While implements Term {
		private Term condition;
		private Block body;

		public While(Term condition, Block body) {
			this.condition = condition;
			this.body = body;
		}

		public Term getCondition() {
			return condition;
		}

		public Block getBody() {
			return body;
		}
	}

	/**
	 * Represents a type in a Java source. Such types do not have to be fully
	 * qualified, though they can be.
	 *
	 * @author David J. Pearce
	 *
	 */
	public interface Type extends Term {

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
