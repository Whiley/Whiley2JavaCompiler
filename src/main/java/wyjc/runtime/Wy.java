package wyjc.runtime;

import java.math.BigInteger;
import java.util.Arrays;

/**
 * Represents the Whiley Runtime
 *
 * @author David J. Pearce
 *
 */
public class Wy {
	public static boolean equals(Object lhs, Object rhs) {
		return lhs.equals(rhs);
	}

	public static boolean equals(boolean[] lhs, boolean[] rhs) {
		return Arrays.equals(lhs, rhs);
	}

	public static boolean equals(byte[] lhs, byte[] rhs) {
		return Arrays.equals(lhs, rhs);
	}

	public static <T> boolean equals(T[] lhs, T[] rhs) {
		return Arrays.deepEquals(lhs, rhs);
	}

	public static void debug(BigInteger[] arr) {
		for(int i=0;i!=arr.length;++i) {
			System.err.print((char) arr[i].intValue());
		}
	}

	/**
	 * Convert an instance of java.lang.String[] into an instance of ascii:string[].
	 *
	 * @param args
	 * @return
	 */
	public static BigInteger[][] toAsciiStrings(String[] args) {
		BigInteger[][] nargs = new BigInteger[args.length][];
		for(int i=0;i!=args.length;++i) {
			nargs[i] = toAsciiString(args[i]);
		}
		return nargs;
	}

	/**
	 * Convert an instance of java.lang.String into an instance of ascii:string.
	 *
	 * @param args
	 * @return
	 */
	public static BigInteger[] toAsciiString(String arg) {
		BigInteger[] narg = new BigInteger[arg.length()];
		for(int i=0;i!=narg.length;++i) {
			narg[i] = BigInteger.valueOf(arg.charAt(i));
		}
		return narg;
	}

	/**
	 * Provides a uniform representation of records in Whiley on the JVM. It should
	 * be pretty obvious that this is not particularly efficient. Some points:
	 *
	 * <ul>
	 * <li><b>Field Accesses</b>. They require a method call and HashMap look up
	 * against a String key.</li>
	 * <li><b>Data Representation</b>. Simple data types like int must be boxed as
	 * Integer when stored in the Struct.</li>
	 * <li><b>Coercions</b>. Coercions between equivalent (though non-identical)
	 * types don't require actual coercions. For example, between Point and the
	 * anonymous record {int x, int y}.</li>
	 * <li><b>Open Records</b>. Open records require no special treatment. They
	 * "just work".</li>
	 * <li><b>Effective Unions</b>. Likewise, unions of records also require no
	 * special treatment. Again, they "just work". Recursion. Recursive types are
	 * supported out-of-the-box without any additional machinery.</li>
	 * </ul>
	 *
	 * @see <a href=
	 *      "https://github.com/Whiley/Whiley2JavaCompiler/issues/19">Issue#19</a>
	 *      for more information.
	 *
	 * @author David J. Pearce
	 *
	 */
	public static final class Struct implements Cloneable {
		private final String schema;
		private final Object[] values;

		public Struct(String schema, Object... values) {
			this.schema = schema;
			this.values = values;
		}

		Struct(Struct copy) {
			this.schema = copy.schema;
			this.values = Arrays.copyOf(copy.values, copy.values.length);
		}

		// ================================================================================
		// Record Operations
		// ================================================================================

		/**
		 * Direct field read. This is possible when the exact object layout is known.
		 * For example, if the type is <code>{int x, int y}</code> then we know field
		 * <code>x</code> has index 0.
		 *
		 * @param index
		 * @return
		 */
		public <T> T get(int index) {
			return (T) values[index];
		}

		/**
		 * Indirect field read. This is necessary the exact object layout is not known.
		 * For example, if the type is <code>{int x, ...}</code> then field
		 * <code>x</code> can be at an arbitrary index.
		 *
		 * @param index
		 * @return
		 */
		public <T> T get(final String field) {
			final int index = indexOf(field);
			return (T) values[index];
		}

		/**
		 * Direct field write. This is possible when the exact object layout is known.
		 * For example, if the type is <code>{int x, int y}</code> then we know field
		 * <code>x</code> has index 0.
		 *
		 * @param index
		 * @return
		 */
		public void put(int index, Object value) {
			values[index] = value;
		}

		/**
		 * Indirect field read. This is necessary the exact object layout is not known.
		 * For example, if the type is <code>{int x, ...}</code> then field
		 * <code>x</code> can be at an arbitrary index.
		 *
		 * @param index
		 * @return
		 */
		public void put(String field, Object value) {
			final int index = indexOf(field);
			values[index] = value;
		}

		// ================================================================================
		// Standard Operations
		// ================================================================================

		@Override
		public boolean equals(Object o) {
			if (o instanceof Struct) {
				Struct s = (Struct) o;
				return schema == s.schema && Arrays.equals(values, s.values);
			}
			return false;
		}

		@Override
		public int hashCode() {
			// FIXME: could include fields as well
			return Arrays.hashCode(values);
		}

		@Override
		public Struct clone() {
			return new Struct(this);
		}

		@Override
		public String toString() {
			StringBuilder builder = new StringBuilder();
			String[] schema = this.schema.split(",");
			builder.append("{");
			for (int i = 0; i != schema.length; ++i) {
				builder.append(schema[i]);
				builder.append(": ");
				builder.append(values[i]);
			}
			builder.append("}");
			return builder.toString();
		}

		// ================================================================================
		// Helper
		// ================================================================================

		private int indexOf(String field) {
			// FIXME: This is a hack. We can do much better than this :)
			String[] schema = this.schema.split(",");
			for (int i = 0; i != schema.length; ++i) {
				if (schema[i].equals(field)) {
					return i;
				}
			}
			System.out.println("SCHEMA: " + Arrays.toString(schema));
			throw new IllegalArgumentException("invalid field access");
		}

	}

}
