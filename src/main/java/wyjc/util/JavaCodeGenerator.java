package wyjc.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import wybs.util.ResolveError;
import wycc.util.Pair;
import wyil.lang.WyilFile;
import wyjc.core.JavaFile;

public class JavaCodeGenerator {

	public static JavaFile.Class generateUnion(String name, JavaFile.Type... bounds) {
		JavaFile.Field tag = new JavaFile.Field(JavaFile.INT, "tag");
		tag.getModifiers().add(JavaFile.Modifier.FINAL);
		tag.getModifiers().add(JavaFile.Modifier.PRIVATE);
		JavaFile.Field content = new JavaFile.Field(JAVA_LANG_OBJECT, "content");
		content.getModifiers().add(JavaFile.Modifier.FINAL);
		content.getModifiers().add(JavaFile.Modifier.PRIVATE);
		JavaFile.Class union = new JavaFile.Class(name);
		union.getModifiers().add(JavaFile.Modifier.STATIC);
		union.getDeclarations().add(tag);
		union.getDeclarations().add(content);
		// Generate constructors
		for(int i=0;i!=bounds.length;++i) {
			union.getDeclarations().add(generateUnionConstructor(name,i, bounds[i]));
		}
		// Generate accessors
		for(int i=0;i!=bounds.length;++i) {
			union.getDeclarations().add(generateUnionTest(i,bounds[i]));
			union.getDeclarations().add(generateUnionAccessor(bounds[i]));
		}
		return union;
	}

	private static JavaFile.Constructor generateUnionConstructor(String name, int tag, JavaFile.Type type) {
		JavaFile.Constructor constructor = new JavaFile.Constructor(name);
		constructor.getParameters().add(new Pair<>(type, "content"));
		//
		JavaFile.Block body = new JavaFile.Block();
		JavaFile.Term thisVar = new JavaFile.VariableAccess("this");
		//
		JavaFile.Term fieldTag = new JavaFile.FieldAccess(thisVar, "tag");
		JavaFile.Term tagConst = new JavaFile.Constant(tag);
		JavaFile.Assignment tagInitialiser = new JavaFile.Assignment(fieldTag, tagConst);
		body.getTerms().add(tagInitialiser);
		//
		JavaFile.Term fieldContent = new JavaFile.FieldAccess(thisVar, "content");
		JavaFile.VariableAccess paramContent = new JavaFile.VariableAccess("content");
		JavaFile.Assignment contentIntialiser = new JavaFile.Assignment(fieldContent, paramContent);
		body.getTerms().add(contentIntialiser);
		//
		constructor.setBody(body);
		constructor.getModifiers().add(JavaFile.Modifier.PUBLIC);
		return constructor;
	}

	private static JavaFile.Method generateUnionTest(int tag, JavaFile.Type type) {
		String name = generateTypeName(type);
		JavaFile.Method constructor = new JavaFile.Method("is" + name, JavaFile.BOOLEAN);
		//
		JavaFile.Block body = new JavaFile.Block();
		JavaFile.Term lhs = new JavaFile.FieldAccess(new JavaFile.VariableAccess("this"), "tag");
		JavaFile.Term rhs = new JavaFile.Constant(tag);
		JavaFile.Term eq = new JavaFile.Operator(JavaFile.Operator.Kind.EQ, lhs, rhs);
		JavaFile.Term ret = new JavaFile.Return(eq);
		body.getTerms().add(ret);
		//
		constructor.setBody(body);
		constructor.getModifiers().add(JavaFile.Modifier.PUBLIC);
		return constructor;
	}

	private static JavaFile.Method generateUnionAccessor(JavaFile.Type type) {
		String name = generateTypeName(type);
		JavaFile.Method constructor = new JavaFile.Method("as" + name, type);
		//
		JavaFile.Block body = new JavaFile.Block();
		JavaFile.Term rhs = new JavaFile.FieldAccess(new JavaFile.VariableAccess("this"), "content");
		JavaFile.Term cast = new JavaFile.Cast(type, rhs);
		JavaFile.Term ret = new JavaFile.Return(cast);
		body.getTerms().add(ret);
		//
		constructor.setBody(body);
		constructor.getModifiers().add(JavaFile.Modifier.PUBLIC);
		return constructor;
	}

	private static String generateTypeName(JavaFile.Type type) {
		if(type instanceof JavaFile.Primitive) {
			JavaFile.Primitive p = (JavaFile.Primitive) type;
			return p.getKind().toString();
		} else if(type instanceof JavaFile.Array) {
			JavaFile.Array p = (JavaFile.Array) type;
			return generateTypeName(p.getElement()) + "_" + "Array";
		} else  {
			JavaFile.Reference p = (JavaFile.Reference) type;
			String[] elements = p.getElements();
			String r = "";
			for(int i=0;i!=p.size();++i) {
				r = r.concat(elements[i]);
			}
			return r;
		}
	}

	public static JavaFile.Class generateStruct(String name, List<JavaFile.Field> fields) {
		JavaFile.Class struct = new JavaFile.Class(name);
		struct.getModifiers().add(JavaFile.Modifier.STATIC);
		struct.getDeclarations().addAll(fields);
		// Write field declartions
		struct.getDeclarations().add(generateConstructor(name, fields));
		struct.getDeclarations().add(generateEqualsMethod(name, fields));
		// typeClass.getDeclarations().add(generateHashcodeMethod(name,fields));
		struct.getDeclarations().add(generateCloneMethod(name, fields));
		return struct;
	}

	private static JavaFile.Constructor generateConstructor(String name, List<JavaFile.Field> fields) {
		JavaFile.Constructor constructor = new JavaFile.Constructor(name);
		List<Pair<JavaFile.Type, String>> parameters = constructor.getParameters();
		for (int i = 0; i != fields.size(); ++i) {
			JavaFile.Field field = fields.get(i);
			parameters.add(new Pair<>(field.getType(), field.getName()));
		}
		JavaFile.Block body = new JavaFile.Block();
		for (int i = 0; i != fields.size(); ++i) {
			String fieldName = fields.get(i).getName();
			JavaFile.Term lhs = new JavaFile.FieldAccess(new JavaFile.VariableAccess("this"), fieldName);
			JavaFile.VariableAccess rhs = new JavaFile.VariableAccess(fieldName);
			JavaFile.Assignment initialiser = new JavaFile.Assignment(lhs, rhs);
			body.getTerms().add(initialiser);
		}
		constructor.setBody(body);
		constructor.getModifiers().add(JavaFile.Modifier.PUBLIC);
		return constructor;
	}

	private static JavaFile.Method generateEqualsMethod(String name, List<JavaFile.Field> fields) {
		JavaFile.VariableAccess otherVar = new JavaFile.VariableAccess("other");
		JavaFile.VariableAccess oVar = new JavaFile.VariableAccess("o");
		// Construct instanceof test
		JavaFile.Type thisType = new JavaFile.Reference(name);
		JavaFile.Term condition = new JavaFile.InstanceOf(otherVar,thisType);
		JavaFile.Block trueBranch = new JavaFile.Block();
		JavaFile.If ifStmt = new JavaFile.If(condition, trueBranch, null);
		// Construct conditional body
		JavaFile.Cast cast = new JavaFile.Cast(thisType,otherVar);
		JavaFile.VariableDeclaration decl = new JavaFile.VariableDeclaration(thisType, "o", cast);
		JavaFile.Term retCondition = null;
		for(int i=0;i!=fields.size();++i) {
			JavaFile.Field field = fields.get(i);
			JavaFile.Term lhs = new JavaFile.VariableAccess(field.getName());
			JavaFile.Term rhs = new JavaFile.FieldAccess(oVar, field.getName());
			JavaFile.Term clause = translateEquality(field.getType(),lhs,rhs);
			retCondition = retCondition == null ? clause
					: new JavaFile.Operator(JavaFile.Operator.Kind.AND, retCondition, clause);
		}
		JavaFile.Return retStmt = new JavaFile.Return(retCondition);
		trueBranch.getTerms().add(decl);
		trueBranch.getTerms().add(retStmt);
		// Construct method body
		JavaFile.Block block = new JavaFile.Block();
		block.getTerms().add(ifStmt);
		block.getTerms().add(new JavaFile.Return(new JavaFile.Constant(false)));
		// Construct method
		JavaFile.Method method = new JavaFile.Method("equals", JavaFile.BOOLEAN);
		method.getModifiers().add(JavaFile.Modifier.PUBLIC);
		method.getParameters().add(new Pair<JavaFile.Type,String>(new JavaFile.Reference("Object"),"other"));
		method.setBody(block);
		//
		return method;
	}

	private static JavaFile.Term translateEquality(JavaFile.Type type, JavaFile.Term lhs, JavaFile.Term rhs) {
		if (type instanceof JavaFile.Primitive) {
			return new JavaFile.Operator(JavaFile.Operator.Kind.EQ, lhs, rhs);
		} else if (type instanceof JavaFile.Array) {
			return new JavaFile.Invoke(null, new String[] { "Array", "equals" }, lhs, rhs);
		} else {
			return new JavaFile.Invoke(lhs, new String[] { "equals" }, rhs);
		}
	}

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
	private static JavaFile.Method generateCloneMethod(String name, List<JavaFile.Field> fields) {
		// First, generate constructor invocation with appropriate cloning for
		// fields.
		List<JavaFile.Term> initialisers = new ArrayList<>();
		for(int i=0;i!=fields.size();++i) {
			JavaFile.Field field = fields.get(i);
			// FIXME: this is completely broken, as it does not recursively
			// clone mutable fields.
			JavaFile.Term lhs = new JavaFile.VariableAccess(field.getName());
			initialisers.add(lhs);
		}
		JavaFile.Reference thisType = new JavaFile.Reference(name);
		JavaFile.New newCall = new JavaFile.New(thisType, initialisers);
		JavaFile.Return retStmt = new JavaFile.Return(newCall);
		JavaFile.Block block = new JavaFile.Block();
		block.getTerms().add(retStmt);
		// Construct the clone method itself
		JavaFile.Method method = new JavaFile.Method("clone", JAVA_LANG_OBJECT);
		method.getModifiers().add(JavaFile.Modifier.PUBLIC);
		method.setBody(block);
		//
		return method;
	}

	private static final JavaFile.Reference JAVA_LANG_OBJECT = new JavaFile.Reference("Object");
}
