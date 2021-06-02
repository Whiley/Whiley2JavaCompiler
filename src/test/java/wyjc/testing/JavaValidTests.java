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

package wyjc.testing;

import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import wybs.lang.Build;
import wybs.lang.SyntaxError;
import wybs.util.AbstractBuildExecutor;
import wybs.util.AbstractBuildRule;
import wybs.util.StdProject;
import wyc.lang.WhileyFile;
import wyc.task.CompileTask;
import wyc.util.TestUtils;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.util.DirectoryRoot;
import wyil.lang.WyilFile;
import wyjc.builder.JavaCompileTask;

/**
 * Run through all valid test cases with verification enabled. Since every test
 * file is valid, a successful test occurs when the compiler succeeds and, when
 * executed, the compiled file produces the expected output. Note that an
 * internal failure does not count as a valid pass, and indicates the test
 * exposed some kind of compiler bug.
 *
 * @author David J. Pearce
 *
 */
@RunWith(Parameterized.class)
public class JavaValidTests {

	/**
	 * Ignored tests and a reason why we ignore them.
	 */
	public final static Map<String, String> IGNORED = new HashMap<>();

	static {
		// =============================================
		// WyC Prexisting problems
		// =============================================
		// Problem Type Checking Union Type
		IGNORED.put("RecordSubtype_Valid_1", "#696");
		IGNORED.put("RecordSubtype_Valid_2", "#696");
		// Function Overloading for Nominal Types
		IGNORED.put("Function_Valid_11", "#702");
		IGNORED.put("Function_Valid_15", "#702");
		//  Normalisation for Method Subtyping
		IGNORED.put("Lifetime_Lambda_Valid_2", "#794");
		IGNORED.put("Lifetime_Lambda_Valid_5", "#794");
		IGNORED.put("Lifetime_Lambda_Valid_6", "#794");
		// Support Captured Lifetime Parameters
		IGNORED.put("Lifetime_Lambda_Valid_7", "#795");
		// Type Tests with Invariants
		IGNORED.put("TypeEquals_Valid_23", "#787");
		IGNORED.put("TypeEquals_Valid_25", "#787");
		IGNORED.put("TypeEquals_Valid_30", "#787");
		IGNORED.put("TypeEquals_Valid_41", "#787");
		// Remove Any and Negation Types
		IGNORED.put("ConstrainedReference_Valid_1", "#827");
		// Temporary Removal of Intersections
		IGNORED.put("Intersection_Valid_1", "#843");
		IGNORED.put("Intersection_Valid_2", "#843");
		IGNORED.put("NegationType_Valid_3", "#843");
		// Problems with relaxed versus strict subtype operator
		IGNORED.put("Function_Valid_22", "#845");
		// Unclassified
		IGNORED.put("Lifetime_Valid_8", "???");
		// Readable Reference Types
		IGNORED.put("UnionType_Valid_26", "#849");
		// Rethinking Runtime Type Test Operator ?
		IGNORED.put("RecordAssign_Valid_11", "#850");
		// Ambiguous coercions
		IGNORED.put("TypeEquals_Valid_33", "#837");
		IGNORED.put("TypeEquals_Valid_35", "#837");
		IGNORED.put("Coercion_Valid_10", "#837");

		// =============================================
		// WyJC Specific problems
		// =============================================

		// Overloaded Union Constructors
		IGNORED.put("Access_Valid_2", "#12");
		IGNORED.put("Array_Valid_1", "#12");
		IGNORED.put("ConstrainedInt_Valid_16", "#12");
		IGNORED.put("ConstrainedList_Valid_2", "#12");
		// Translating Lambdas
		IGNORED.put("AddressExpression_Valid_1", "#13");
		IGNORED.put("AddressExpression_Valid_2", "#13");
		IGNORED.put("AddressExpression_Valid_3", "#13");
		IGNORED.put("AddressExpression_Valid_4", "#13");
		// Access After Clone
		IGNORED.put("BoolRecord_Valid_2", "#15");
		IGNORED.put("ConstrainedInt_Valid_11", "#15");
		IGNORED.put("ConstrainedNegation_Valid_2", "#15");
		IGNORED.put("ConstrainedRecord_Valid_1", "#15");
		// Runtime Type Tests
		IGNORED.put("Coercion_Valid_7", "#18");
		IGNORED.put("Coercion_Valid_8", "#18");
		IGNORED.put("Coercion_Valid_9", "#18");
	}

	/**
	 * The directory containing the source files for each test case. Every test
	 * corresponds to a file in this directory.
	 */
	public final static String WHILEY_SRC_DIR = "tests/valid".replace('/', File.separatorChar);

	// ======================================================================
	// Test Harness
	// ======================================================================

	/**
 	 * Compile a syntactically invalid test case with verification enabled. The
 	 * expectation is that compilation should fail with an error and, hence, the
 	 * test fails if compilation does not.
 	 *
 	 * @param name
 	 *            Name of the test to run. This must correspond to a whiley
 	 *            source file in the <code>WHILEY_SRC_DIR</code> directory.
	 * @throws IOException
 	 */
 	protected void runTest(String name) throws IOException {
		// Compile to Java Bytecode
		Pair<Boolean, String> p = compileWhiley2Java(
				WHILEY_SRC_DIR, // location of source directory
				name); // name of test to compile

		boolean r = p.first();

		System.out.print(p.second());

		if (!r) {
			fail("Test failed to compile!");
		}
		// compile the generated Java Program.
		if (!compileJava2Bytecode(WHILEY_SRC_DIR, name + ".java")) {
			fail("unable to compile Java file");
		} else {
			// execute the generated Java Program.
			String CLASSPATH = System.getProperty("java.class.path");
			// FIXME: if generated classfiles were dumped into target/classes,
			// then
			// we wouldn't need to append the following onto CLASSPATH.
			CLASSPATH = WHILEY_SRC_DIR + File.pathSeparator + CLASSPATH;
			String output = exec(CLASSPATH, ".", "wyjc.testing.JavaValidTests", name);
			if (!output.equals("")) {
				System.out.println(output);
				fail("unexpected output!");
			}
		}
 	}

 	/**
	 * A simple default registry which knows about whiley files and wyil files.
	 */
	private static final Content.Registry registry = new TestUtils.Registry();

 	/**
	 * Run the Whiley Compiler with the given list of arguments ot produce a
	 * Java source file. This will then need to be separately compiled.
	 *
	 * @param args
	 *            --- list of command-line arguments to provide to the Whiley
	 *            Compiler.
	 * @return
	 * @throws IOException
	 */
	public static Pair<Boolean,String> compileWhiley2Java(String whileydir, String... args) throws IOException {
		ByteArrayOutputStream syserr = new ByteArrayOutputStream();
		ByteArrayOutputStream sysout = new ByteArrayOutputStream();
		//
		boolean result = true;
		//
		try {
			// Construct the project
			DirectoryRoot root = new DirectoryRoot(whileydir, registry);
			StdProject project = new StdProject(root);
			// Add build rules
			addCompilationRules(project,root,false);
			// Create empty build graph
			Build.Executor graph = new AbstractBuildExecutor();
			// Identify source files and build project
			project.build(TestUtils.findSourceFiles(root,graph,args), graph);
			// Flush any created resources (e.g. wyil files)
			root.flush();
		} catch (SyntaxError e) {
			// Print out the syntax error
			e.outputSourceError(new PrintStream(syserr),false);
			result = false;
		} catch (Exception e) {
			// Print out the syntax error
			e.printStackTrace(new PrintStream(syserr));
			result = false;
		}
		// Convert bytes produced into resulting string.
		byte[] errBytes = syserr.toByteArray();
		byte[] outBytes = sysout.toByteArray();
		String output = new String(errBytes) + new String(outBytes);
		return new Pair<>(result, output);
	}

	/**
	 * Add build rules to project for compiling from Whiley source to Java. This is
	 * a two step process whereby we first using the Whiley Compiler (WyC) to
	 * generate wyil files, and then use the Java compiler for the final stage.
	 *
	 * @param project
	 * @param root
	 * @param verify
	 */
	private static void addCompilationRules(StdProject project, Path.Root root, boolean verify) {
		CompileTask wyTask = new CompileTask(project);
		JavaCompileTask javaTask = new JavaCompileTask(project);
		// Add compilation rule(s) (whiley => wyil)
		project.add(new AbstractBuildRule(wyTask, root, Content.filter("**", WhileyFile.ContentType), null, root));
		// Add compilation rule(s) (wyil => js)
		project.add(new AbstractBuildRule(javaTask, root, Content.filter("**", WyilFile.ContentType), null, root));
	}

	/**
	 * Compile a Java program to JVM bytecode
	 *
	 * @param whileydir
	 * @param args
	 * @return
	 * @throws IOException
	 */
	public static boolean compileJava2Bytecode(String whileydir, String javaFilename) throws IOException {
		try {
			// Make use of java compiler API
			final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
			final DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
			final StandardJavaFileManager manager = compiler.getStandardFileManager(diagnostics, null, null);
			final File file = new File(whileydir + File.separatorChar + javaFilename);
			final Iterable<? extends JavaFileObject> sources = manager.getJavaFileObjectsFromFiles(Arrays.asList(file));
			final CompilationTask task = compiler.getTask(null, manager, diagnostics, null, null, sources);
			if (task.call()) {
				return true;
			} else {
				for (final Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
					System.out.format("%s, line %d in %s\n", diagnostic.getMessage(null), diagnostic.getLineNumber(),
							diagnostic.getSource().getName());
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			fail("Problem running compiled test");
		}

		return false;
	}

	/**
	 * Execute a given class file using the "java" command, and return all
	 * output written to stdout. In the case of some kind of failure, write the
	 * generated stderr stream to this processes stdout.
	 *
	 * @param classPath
	 *            Class path to use when executing Java code. Note, directories
	 *            can always be safely separated with '/', and path separated
	 *            with ':'.
	 * @param srcDir
	 *            Path to root of package containing class. Note, directories
	 *            can always be safely separated with '/'.
	 * @param className
	 *            Name of class to execute
	 * @param args
	 *            Arguments to supply on the command-line.
	 * @return All output generated from the class that was written to stdout.
	 */
	public static String exec(String classPath, String srcDir, String className, String... args) {
		try {
			classPath = classPath.replace('/', File.separatorChar);
			classPath = classPath.replace(':', File.pathSeparatorChar);
			srcDir = srcDir.replace('/', File.separatorChar);
			String tmp = "java -ea -cp " + classPath + " " + className;
			for (String arg : args) {
				tmp += " " + arg;
			}
			Process p = Runtime.getRuntime().exec(tmp, null, new File(srcDir));

			StringBuffer syserr = new StringBuffer();
			StringBuffer sysout = new StringBuffer();
			new TestUtils.StreamGrabber(p.getErrorStream(), syserr);
			new TestUtils.StreamGrabber(p.getInputStream(), sysout);
			int exitCode = p.waitFor();
			if (exitCode != 0) {
				System.err
						.println("============================================================");
				System.err.println(className);
				System.err
						.println("============================================================");
				System.err.println(syserr);
				return null;
			} else {
				return sysout.toString();
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			fail("Problem running compiled test");
		}

		return null;
	}


 	/**
	 * This is the entry point for each test. The argument provided is the name
	 * of the test class. The special method "test" is then invoked on this
	 * class with no arguments provided. The method should execute without
	 * producing output (success), or report some kind of runtime fault
	 * (failure).
	 *
	 * @param args
	 */
 	public static void main(String[] args) {
 		String testClassName = args[0];
 		try {
 			Class testClass = Class.forName(testClassName);
 			Method testMethod = testClass.getMethod("test");
 			testMethod.invoke(null);
 		} catch(ClassNotFoundException e) {
 			e.printStackTrace(System.out);
 		} catch(NoSuchMethodException e) {
 			e.printStackTrace(System.out);
 		} catch(InvocationTargetException e) {
 			e.printStackTrace(System.out);
 		} catch (IllegalAccessException e) {
			e.printStackTrace(System.out);
		}
 	}

	/**
	 * Construct a classpath string from a sequence of path components. Each
	 * classpath component is separated by ':' (note that
	 * <code>TestUtils.exec</code> takes care of platform dependent path
	 * separators).
	 *
	 * @param components
	 * @return
	 */
	public static String CLASSPATH(String... components) {
		String r = "";
		boolean firstTime = true;
		for (String c : components) {
			if (!firstTime) {
				r = r + ":";
			}
			firstTime = false;
			r = r + c;
		}
		return r;
	}

	// ======================================================================
	// Tests
	// ======================================================================

	// Parameter to test case is the name of the current test.
	// It will be passed to the constructor by JUnit.
	private final String testName;
	public JavaValidTests(String testName) {
		this.testName = testName;
	}

	// Here we enumerate all available test cases.
	@Parameters(name = "{0}")
	public static Collection<Object[]> data() {
		return TestUtils.findTestNames(WHILEY_SRC_DIR);
	}

	// Skip ignored tests
	@Before
	public void beforeMethod() {
		String ignored = IGNORED.get(this.testName);
		Assume.assumeTrue("Test " + this.testName + " skipped: " + ignored, ignored == null);
	}

	@Test
	public void valid() throws IOException {
		runTest(this.testName);
	}
}
