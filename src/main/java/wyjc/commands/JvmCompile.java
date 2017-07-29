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
import wyc.commands.Compile.Result;
import wyc.lang.WhileyFile;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.util.DirectoryRoot;
import wyil.lang.WyilFile;
import wyjc.builder.JvmCompileTask;

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

	public void setClassdir(File dir) throws IOException {
		this.classdir = new DirectoryRoot(dir,registry);
	}

	@Override
	protected void finaliseConfiguration() throws IOException {
		super.finaliseConfiguration();
		this.classdir = getDirectoryRoot(classdir,wyildir);
	}

	@Override
	protected Result compile(StdProject project, List<Path.Entry<WhileyFile>> entries) {
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
		Content.Filter<WyilFile> wyilIncludes = Content.filter("**", WyilFile.ContentType);
		Content.Filter<WyilFile> wyilExcludes = null;
		// Rule for compiling Whiley to WyIL
		JvmCompileTask jvmBuilder = new JvmCompileTask(project);
		if(verbose) {
			jvmBuilder.setLogger(logger);
		}
		// FIXME: should be able to set class directory
		project.add(new StdBuildRule(jvmBuilder, wyildir, wyilIncludes, wyilExcludes, classdir));
	}
}
