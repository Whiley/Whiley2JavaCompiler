// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.commands;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import wybs.util.StdProject;
import wyc.command.Compile;
import wyc.lang.WhileyFile;
import wycc.util.ArrayUtils;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.util.DirectoryRoot;
import wyjc.Activator;

public class JvmCompile extends Compile {

	/**
	 * The location in which class binary files are stored, or null if not
	 * specified.
	 */
	protected DirectoryRoot classdir;

	/**
	 * Construct a new instance of this command.
	 *
	 * @param registry
	 *            The content registry being used to match files to content
	 *            types.
	 * @throws IOException
	 */
	public JvmCompile(Content.Registry registry, Logger logger) {
		super(registry, logger);
	}

	/**
	 * Construct a new instance of this command.
	 *
	 * @param registry
	 *            The content registry being used to match files to content
	 *            types.
	 * @throws IOException
	 */
	public JvmCompile(Content.Registry registry, Logger logger, OutputStream sysout, OutputStream syserr) {
		super(registry, logger, sysout, syserr);
	}


	@Override
	public String getName() {
		return "jvmcompile";
	}

	@Override
	public String getDescription() {
		return "Compile Whiley source files to JVM class files";
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

	@Override
	protected void finaliseConfiguration() throws IOException {
		super.finaliseConfiguration();
		this.classdir = getDirectoryRoot(classdir,wyildir);
	}

	@Override
	protected Result compile(StdProject project, List<? extends Path.Entry<?>> entries) {
		try {
			Result r = super.compile(project, entries);
			classdir.flush();
			return r;
		} catch (IOException e) {
			// now what?
			throw new RuntimeException(e);
		}
	}

	/**
	 * Add build rules necessary for compiling whiley source files into binary
	 * wyil files.
	 *
	 * @param project
	 */
	@Override
	protected void addCompilationBuildRules(StdProject project) {
		super.addCompilationBuildRules(project);
		addWyil2JvmBytecodeBuildRule(project);
	}

	protected void addWyil2JvmBytecodeBuildRule(StdProject project) {
		// Configure build rules for normal compilation
		Content.Filter<WhileyFile> wyilIncludes = Content.filter("**", WhileyFile.BinaryContentType);
		Content.Filter<WhileyFile> wyilExcludes = null;
		// Rule for compiling Whiley to WyIL
//		JvmCompileTask jvmBuilder = new JvmCompileTask(project);
//		if(verbose) {
//			jvmBuilder.setLogger(logger);
//		}
//		// FIXME: should be able to set class directory
//		project.add(new StdBuildRule(jvmBuilder, wyildir, wyilIncludes, wyilExcludes, classdir));
	}

	@Override
	public List<? extends Path.Entry<?>> getModifiedSourceFiles() throws IOException {
		return getModifiedSourceFiles(wyildir, wyilIncludes, classdir, Activator.ContentType);
	}
}
