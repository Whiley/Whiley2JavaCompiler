package wyjc.commands;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import wybs.util.StdProject;
import wyc.util.AbstractProjectCommand;
import wycc.util.ArrayUtils;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyfs.util.DirectoryRoot;

public class JvmRun extends AbstractProjectCommand<JvmRun.Result> {
	/**
	 * Result kind for this command
	 *
	 */
	public enum Result {
		SUCCESS,
		ERRORS,
		INTERNAL_FAILURE
	}

	/**
	 * The location in which class binary files are stored, or null if not
	 * specified.
	 */
	protected DirectoryRoot classdir;

	public JvmRun(Content.Registry registry, Logger logger) {
		super(registry, logger);
	}

	// =======================================================================
	// Configuration
	// =======================================================================

	@Override
	public String getDescription() {
		return "Execute a given classfile (requires main method)";
	}

	@Override
	public String getName() {
		return "jvmrun";
	}

	private static final String[] SCHEMA = {
			"classdir"
	};

	@Override
	public String[] getOptions() {
		return ArrayUtils.append(super.getOptions(),SCHEMA);
	}

	@Override
	public void set(String option, Object value) throws ConfigurationError {
		try {
			switch(option) {
			case "classdir":
				setClassdir(new File((String)value));
				break;
			default:
				super.set(option, value);
			}
		} catch(IOException e) {
			throw new ConfigurationError(e);
		}
	}

	@Override
	public String describe(String option) {
		switch(option) {
		case "classdir":
			return "Specify where to place generated class files";
		default:
			return super.describe(option);
		}
	}

	public void setClassdir(File dir) throws IOException {
		this.classdir = new DirectoryRoot(dir,registry);
	}
	// =======================================================================
	// Execute
	// =======================================================================

	@Override
	public Result execute(String... args) {
		if (args.length < 1) {
			// FIXME: this is broken
			System.out.println("usage: jvmrun <classfile>");
			return Result.ERRORS;
		}
		try {
			// FIXME: need to actually load the class from the project!!
			StdProject project = initialiseProject();
			// Load class
			Class<?> instance = Class.forName(args[0]);
			// Construct arguments
			String[] nArgs = new String[args.length-1];
			System.arraycopy(args, 1, nArgs, 0, nArgs.length);
			// Invoke method
			Method method = instance.getMethod("main", String[].class);
			method.invoke(null, new Object[]{nArgs});
		} catch (IOException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (ClassNotFoundException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (NoSuchMethodException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (SecurityException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (IllegalArgumentException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		} catch (InvocationTargetException e) {
			// FIXME: this is broken
			throw new RuntimeException(e);
		}
		return Result.SUCCESS;
	}


	@Override
	protected void finaliseConfiguration() throws IOException {
		super.finaliseConfiguration();
		this.classdir = getDirectoryRoot(classdir,wyildir);
	}

}
