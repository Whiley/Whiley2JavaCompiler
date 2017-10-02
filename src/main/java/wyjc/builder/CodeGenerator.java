package wyjc.builder;

import java.util.Collection;
import wyjc.core.JavaFile;

/**
 * A code generator is responsible for generating code of a specific type. For
 * example, it might be responsible for generating an additional method or
 * class. We can think of this as a parameterised component of the compiler.
 *
 * @author David J. Pearce
 *
 */
public interface CodeGenerator<T> {

	/**
	 * Generate any supplementary declarations which are required for some term
	 * which has been previously generated. For example, if that term relied on a
	 * helper method then we need to generate that method.
	 *
	 * @return
	 */
	public Collection<? extends JavaFile.Declaration> generateSupplementaryDeclarations();
}
