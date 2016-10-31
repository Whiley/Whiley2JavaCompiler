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
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import wyc.commands.Compile;
import wyc.util.TestUtils;
import wycc.util.Logger;
import wycc.util.Pair;
import wyfs.lang.Content;
import wyjc.commands.JvmCompile;

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
public class RuntimeValidTests {

	/**
	 * Ignored tests and a reason why we ignore them.
	 */
	public final static Map<String, String> IGNORED = new HashMap<String, String>();

	static {
		IGNORED.put("Coercion_Valid_8", "#681");
		IGNORED.put("Complex_Valid_3", "Issue ???");
		IGNORED.put("Complex_Valid_4", "#681");
		IGNORED.put("ConstrainedIntersection_Valid_1", "unknown");
		IGNORED.put("ConstrainedNegation_Valid_1", "#342");
		IGNORED.put("ConstrainedNegation_Valid_2", "#342");
		IGNORED.put("DoWhile_Valid_4", "#681");
		IGNORED.put("FunctionRef_Valid_10", "#663");
		IGNORED.put("FunctionRef_Valid_11", "#663");
		IGNORED.put("FunctionRef_Valid_13", "#555");
		IGNORED.put("Function_Valid_11", "unknown");
		IGNORED.put("Function_Valid_15", "unknown");
		IGNORED.put("IfElse_Valid_4", "#663");
		IGNORED.put("Import_Valid_4", "#492");
		IGNORED.put("Import_Valid_5", "#492");
		IGNORED.put("Intersection_Valid_2", "Issue ???");
		IGNORED.put("Lifetime_Lambda_Valid_4", "#641");
		IGNORED.put("ListAccess_Valid_6", "Issue ???");
		IGNORED.put("OpenRecord_Valid_5", "#663");
		IGNORED.put("OpenRecord_Valid_6", "#664");
		IGNORED.put("RecursiveType_Valid_2", "#663");
		IGNORED.put("RecordSubtype_Valid_1", "Issue ???");
		IGNORED.put("RecordSubtype_Valid_2", "Issue ???");
		IGNORED.put("RecursiveType_Valid_12", "#339");
		IGNORED.put("RecursiveType_Valid_21", "#663");
		IGNORED.put("RecursiveType_Valid_22", "#339");
		IGNORED.put("RecursiveType_Valid_28", "#364");
		IGNORED.put("RecursiveType_Valid_3", "#681");
		IGNORED.put("RecursiveType_Valid_4", "#406");
		IGNORED.put("RecursiveType_Valid_5", "#18");
		IGNORED.put("TypeEquals_Valid_23", "Issue ???");
		IGNORED.put("TypeEquals_Valid_24", "#681");
		IGNORED.put("TypeEquals_Valid_29", "#681");
		IGNORED.put("TypeEquals_Valid_34", "#681");
		IGNORED.put("TypeEquals_Valid_36", "Issue ???");
		IGNORED.put("TypeEquals_Valid_37", "Issue ???");
		IGNORED.put("TypeEquals_Valid_38", "Issue ???");
		IGNORED.put("TypeEquals_Valid_41", "Issue ???");
		IGNORED.put("TypeEquals_Valid_42", "#681");
		IGNORED.put("TypeEquals_Valid_47", "#681");
		IGNORED.put("While_Valid_15", "#681");
		IGNORED.put("While_Valid_20", "unknown");
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
 		// this will need to turn on verification at some point.
 		String whileyFilename = WHILEY_SRC_DIR + File.separatorChar + name + ".whiley";

		// Compile to Java Bytecode
		Pair<Compile.Result, String> p = compile(
				WHILEY_SRC_DIR, // location of source directory
				whileyFilename); // name of test to compile

		Compile.Result r = p.first();

		System.out.print(p.second());

		if (r == Compile.Result.INTERNAL_FAILURE) {
			fail("Test caused internal failure!");
		} else if (r != Compile.Result.SUCCESS) {
			fail("Test failed to compile!");
		}

 		// execute the generated Java Program.
 		String CLASSPATH=System.getProperty("java.class.path");
		// FIXME: if generated classfiles were dumped into target/classes, then
		// we wouldn't need to append the following onto CLASSPATH.
 		CLASSPATH = WHILEY_SRC_DIR + File.pathSeparator + CLASSPATH;
 		String output = exec(CLASSPATH,".","wyjc.testing.RuntimeValidTests",name);
 		if(!output.equals("")) {
 			System.out.println(output);
 			fail("unexpected output!");
 		}
 	}

 	/**
	 * Run the Whiley Compiler with the given list of arguments.
	 *
	 * @param args
	 *            --- list of command-line arguments to provide to the Whiley
	 *            Compiler.
	 * @return
	 * @throws IOException
	 */
	public static Pair<Compile.Result,String> compile(String whileydir, String... args) throws IOException {
		ByteArrayOutputStream syserr = new ByteArrayOutputStream();
		ByteArrayOutputStream sysout = new ByteArrayOutputStream();
		Content.Registry registry = new wyc.Activator.Registry();
		JvmCompile cmd = new JvmCompile(registry,Logger.NULL,sysout,syserr);
		cmd.setWhileydir(whileydir);
		cmd.setVerbose();
		Compile.Result result = cmd.execute(args);
		byte[] errBytes = syserr.toByteArray();
		byte[] outBytes = sysout.toByteArray();
		String output = new String(errBytes) + new String(outBytes);
		return new Pair<Compile.Result,String>(result,output);
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
			String tmp = "java -cp " + classPath + " " + className;
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
	public RuntimeValidTests(String testName) {
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
