// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//    * Neither the name of the <organization> nor the
//      names of its contributors may be used to endorse or promote products
//      derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL DAVID J. PEARCE BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package wyjc.runtime;

import java.util.*;

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
 * @see
 * <a href="https://github.com/Whiley/Whiley2JavaCompiler/issues/19">Issue#19</a>
 * for more information.
 *
 * @author David J. Pearce
 *
 */
public final class Struct implements Cloneable {
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
		for(int i=0;i!=schema.length;++i) {
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
