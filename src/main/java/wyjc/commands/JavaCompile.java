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

import wybs.util.StdBuildRule;
import wybs.util.StdProject;
import wyc.commands.Compile;
import wycc.lang.Feature.ConfigurationError;
import wycc.util.ArrayUtils;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.util.DirectoryRoot;
import wyil.lang.WyilFile;
import wyjc.Activator;
import wyjc.builder.JavaCompileTask;
import wyjc.core.JavaFile;

public class JavaCompile extends Compile {
	/**
	 * The location in which generated java files are stored, or null if not
	 * specified.
	 */
	protected DirectoryRoot javadir;

	/**
	 * Construct a new instance of this command.
	 *
	 * @param registry
	 *            The content registry being used to match files to content
	 *            types.
	 * @throws IOException
	 */
	public JavaCompile(Content.Registry registry, Logger logger) {
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
	public JavaCompile(Content.Registry registry, Logger logger, OutputStream sysout, OutputStream syserr) {
		super(registry, logger, sysout, syserr);
	}

	@Override
	public String getName() {
		return "javacompile";
	}

	@Override
	public String getDescription() {
		return "Compile Whiley source files to Java source files";
	}

	private static final String[] SCHEMA = {
			"javadir"
	};

	@Override
	public String[] getOptions() {
		return ArrayUtils.append(super.getOptions(),SCHEMA);
	}

	@Override
	public void set(String option, Object value) throws ConfigurationError {
		try {
			switch(option) {
			case "javadir":
				setJavadir(new File((String)value));
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
		case "javadir":
			return "Specify where to place generated java files";
		default:
			return super.describe(option);
		}
	}

	public void setJavadir(File dir) throws IOException {
		this.javadir = new DirectoryRoot(dir,registry);
	}

	@Override
	protected void finaliseConfiguration() throws IOException {
		super.finaliseConfiguration();
		this.javadir = getDirectoryRoot(javadir,wyildir);
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
		addWyil2JavaBytecodeBuildRule(project);
	}

	protected void addWyil2JavaBytecodeBuildRule(StdProject project) {
		// Configure build rules for normal compilation
		Content.Filter<WyilFile> wyilIncludes = Content.filter("**", WyilFile.ContentType);
		Content.Filter<WyilFile> wyilExcludes = null;
		// Rule for compiling Whiley to WyIL
		JavaCompileTask javaBuilder = new JavaCompileTask(project);
		if(verbose) {
			javaBuilder.setLogger(logger);
		}
		// FIXME: should be able to set class directory
		project.add(new StdBuildRule(javaBuilder, wyildir, wyilIncludes, wyilExcludes, javadir));
	}


	@Override
	public List<? extends Path.Entry<?>> getModifiedSourceFiles() throws IOException {
		return getModifiedSourceFiles(wyildir, wyilIncludes, javadir, JavaFile.ContentType);
	}
}
