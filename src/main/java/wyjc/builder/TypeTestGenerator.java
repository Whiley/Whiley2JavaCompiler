package wyjc.builder;

import static wyil.lang.WyilFile.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import wyil.lang.WyilFile;
import wyil.lang.WyilFile.Decl;
import wyil.lang.WyilFile.Expr;
import wyil.lang.WyilFile.Type;
import wyjc.core.JavaFile;
import wyjc.core.JavaFile.Declaration;
import wyjc.core.JavaFile.Term;

/**
 * <p>
 * Translate a type test expression. This is a complex procedure which may need
 * to recursively explore the value being tested. Initially, it will employ an
 * inline test whereever possible. For example, consider this case:
 * </p>
 *
 * <pre>
 * function f(int|null x) -> (int r):
 *    if x is int:
 *       ...
 * </pre>
 *
 * <p>
 * Here, we can translate <code>x is int</code> into
 * <code>x instanceof BigInteger</code>. However, not all tests can be
 * translated inline. For example, consider this test:
 * </p>
 *
 * <pre>
 * function f(any[] xs) -> (int r):
 *     if xs is int[]:
 *        ...
 * </pre>
 *
 * <p>
 * This necessarily requires iterating every element of <code>xs</code> to check
 * whether or not it's a BigInteger. This test translates into
 * <code>isType$ia(xs)</code>, defined as:
 * </p>
 *
 * <pre>
 * private static boolean isType$ia(Object[] xs) {
 * 	for (int i = 0; i != xs.length; ++i) {
 * 		if (!xs[i] instanceof BigInteger) {
 * 			return false;
 * 		}
 * 	}
 * 	return true;
 * }
 * </pre>
 * <p>
 * Observe that there are optimisations which can be employed. For example,
 * consider this case:
 * </p>
 *
 * <pre>
 * function f(int|(int[]) xs) -> (int r):
 *     if xs is int[]:
 *        ...
 * </pre>
 * <p>
 * Here, we can translate the test into <code>!(xs instanceof BigInteger)</code>
 * based on the type information we have available.
 * </p>
 *
 * @param expr
 * @param parent
 * @return
 */
public class TypeTestGenerator implements CodeGenerator<Expr.Is> {
	private HashMap<Type,JavaFile.Method> declarations = new HashMap<>();

	/**
	 * <p>
	 * Generate a test method for a given type. For example, if we had
	 * <code>x is int[]</code> then we would generate the following:
	 * </p>
	 *
	 * <pre>
	 * boolean is$1(Object o) {
	 *   if(o is Object[]) {
	 *      Object[] tmp = (Object[]) o;
	 *      for(int i=0;i!=tmp.length;++i) {
	 *         if(!tmp[i] instanceof BigInteger) {
	 *            return false;
	 *         }
	 *      }
	 *      return true;
	 *   }
	 * }
	 * </pre>
	 *
	 * <p>
	 * <b>NOTE:</b> One of the limitations of this implementation is that it does
	 * not take into account knowledge of the variables type. For example, if we
	 * knew x had type <code>any[]</code> then the input parameter to the method
	 * above could be <code>Object[]</code> already.
	 * </p>
	 *
	 * @param type
	 * @param id
	 * @return
	 */
	public Term generate(WyilFile.Type type, JavaFile.Term operand) {
		JavaFile.Block body;
		// First, attempt inline translation (if possible)
		switch (type.getOpcode()) {
		case TYPE_null:
			// Translate "x is null" as "x == null" in Java.
			return new JavaFile.Operator(JavaFile.Operator.Kind.EQ, operand, new JavaFile.Constant(null));
		case TYPE_bool:
			return new JavaFile.InstanceOf(operand, JAVA_LANG_BOOLEAN);
		case TYPE_byte:
			return new JavaFile.InstanceOf(operand, JAVA_LANG_BYTE);
		case TYPE_int:
			return new JavaFile.InstanceOf(operand, JAVA_MATH_BIGINTEGER);
		case TYPE_array:
			body = generateTypeTest((Type.Array) type);
			break;
		case TYPE_record:
			body = generateTypeTest((Type.Record) type);
			break;
		case TYPE_nominal:
			body = generateTypeTest((Type.Nominal) type);
			break;
		default:
			throw new IllegalArgumentException("invalid type argument");
		}
		// At this point, we need a more complex translation. Therefore, we generate an
		// external type test method and return an invocation to that, rather than
		// inlining the test.
		JavaFile.Method method = new JavaFile.Method("is$" + type.getIndex(), JavaFile.BOOLEAN);
		method.getParameters().add(new JavaFile.VariableDeclaration(JAVA_LANG_OBJECT, "o"));
		method.getModifiers().add(JavaFile.Modifier.PRIVATE);
		method.getModifiers().add(JavaFile.Modifier.STATIC);
		method.setBody(body);
		declarations.put(type, method);
		return new JavaFile.Invoke(null, method.getName(), operand);
	}

	private JavaFile.Block generateTypeTest(Type.Record type) {
		List<JavaFile.Term> stmts = new ArrayList<>();
		JavaFile.Term var_r = new JavaFile.VariableAccess("r");
		JavaFile.Term var_v = new JavaFile.VariableAccess("v");
		Tuple<Type.Field> fields = type.getFields();
		for (int i = 0; i != fields.size(); ++i) {
			Type.Field field = fields.get(i);
			JavaFile.Term fieldTest = new JavaFile.Invoke(var_v, "has", new JavaFile.Constant(field.getName().get()));
			JavaFile.Term access = new JavaFile.Invoke(var_v, "get", new JavaFile.Constant(field.getName().get()));
			JavaFile.Term typeTest = generate(field.getType(), access);
			JavaFile.Term rhs = and(fieldTest, typeTest);
			rhs = (i != 0) ? and(var_r, rhs) : rhs;
			JavaFile.Assignment stmt = new JavaFile.Assignment(var_r, rhs);
			stmts.add(stmt);
		}
		return generateTestTemplate(WHILEY_RECORD, stmts);
	}

	private JavaFile.Block generateTypeTest(Type.Array type) {
		List<JavaFile.Term> stmts = new ArrayList<>();
		JavaFile.Term var_r = new JavaFile.VariableAccess("r");
		JavaFile.Term var_v = new JavaFile.VariableAccess("v");
		JavaFile.VariableDeclaration initialiser = new JavaFile.VariableDeclaration(JavaFile.INT, "i", new JavaFile.Constant(0));
		JavaFile.Term var_i = new JavaFile.VariableAccess("i");
		JavaFile.Term length = new JavaFile.FieldAccess(var_v, "length");
		JavaFile.Term condition = new JavaFile.Operator(JavaFile.Operator.Kind.LT, var_i, length);
		JavaFile.Term increment = new JavaFile.Assignment(var_i,
				new JavaFile.Operator(JavaFile.Operator.Kind.ADD, var_i, new JavaFile.Constant(1)));
		JavaFile.Term access = new JavaFile.ArrayAccess(var_v, var_i);
		JavaFile.Term assignment = new JavaFile.Assignment(var_r, and(var_r, generate(type.getElement(),access)));
		JavaFile.Block body = new JavaFile.Block();
		body.getTerms().add(assignment);
		stmts.add(new JavaFile.Assignment(var_r, new JavaFile.Constant(true)));
		stmts.add(new JavaFile.For(initialiser, condition, increment, body));
		return generateTestTemplate(WHILEY_ARRAY, stmts);
	}

	private JavaFile.Block generateTypeTest(Type.Nominal type) {
		// FIXME: problem here with recursive definitions.
		// FIXME: problem here with type invariants
		JavaFile.Block block = new JavaFile.Block();
		JavaFile.Term var_o = new JavaFile.VariableAccess("o");
		JavaFile.Term condition = generate(type.getDeclaration().getType(), var_o);
		block.getTerms().add(new JavaFile.Return(condition));
		return block;
	}

	/**
	 * Generate a test template which looks like this:
	 *
	 * <pre>
	 * boolean r = false;
	 * if(o instanceof TYPE) {
	 *   Type v = (Type) o;
	 *   ...
	 * }
	 * return r;
	 * </pre>
	 *
	 * Here, the TYPE and the true branch contents are parameters.
	 *
	 * @param terms
	 * @return
	 */
	private JavaFile.Block generateTestTemplate(JavaFile.Type type,List<JavaFile.Term> terms) {
		JavaFile.Block block = new JavaFile.Block();
		JavaFile.Term var_r = new JavaFile.VariableAccess("r");
		block.getTerms().add(new JavaFile.VariableDeclaration(JavaFile.BOOLEAN, "r", new JavaFile.Constant(false)));
		JavaFile.Term var_o = new JavaFile.VariableAccess("o");
		JavaFile.Term condition = new JavaFile.InstanceOf(var_o, type);
		// True Branch
		JavaFile.Block trueBranch = new JavaFile.Block();
		trueBranch.getTerms()
				.add(new JavaFile.VariableDeclaration(type, "v", new JavaFile.Cast(type, var_o)));
		trueBranch.getTerms().addAll(terms);
		JavaFile.If stmt = new JavaFile.If(condition, trueBranch, null);
		block.getTerms().add(stmt);
		// False Branch
		block.getTerms().add(new JavaFile.Return(var_r));
		return block;
	}

	@Override
	public Collection<? extends Declaration> generateSupplementaryDeclarations() {
		return declarations.values();

	}

	private static JavaFile.Term and(JavaFile.Term lhs, JavaFile.Term rhs) {
		if (lhs == null) {
			return rhs;
		} else {
			return new JavaFile.Operator(JavaFile.Operator.Kind.AND, lhs, rhs);
		}
	}

	private static JavaFile.Reference JAVA_MATH_BIGINTEGER = new JavaFile.Reference("BigInteger");
	private static JavaFile.Reference JAVA_LANG_OBJECT = new JavaFile.Reference("Object");
	private static JavaFile.Reference JAVA_LANG_BOOLEAN = new JavaFile.Reference("Boolean");
	private static JavaFile.Reference JAVA_LANG_BYTE = new JavaFile.Reference("Byte");
	private static final JavaFile.Reference WHILEY_RECORD = new JavaFile.Reference("Wy", "Struct");
	private static final JavaFile.Array WHILEY_ARRAY = new JavaFile.Array(new JavaFile.Reference("Object"));
}
