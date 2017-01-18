// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.commands;

import java.io.IOException;
import java.io.OutputStream;

import wybs.util.StdBuildRule;
import wybs.util.StdProject;
import wyc.commands.Compile;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyil.lang.WyilFile;
import wyjc.builder.JavaCompileTask;

public class JavaCompile extends Compile {
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
		project.add(new StdBuildRule(javaBuilder, wyildir, wyilIncludes, wyilExcludes, wyildir));
	}
}
