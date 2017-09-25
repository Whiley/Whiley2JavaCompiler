// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// This software may be modified and distributed under the terms
// of the BSD license.  See the LICENSE file for details.

package wyjc.util;

import java.io.File;
import java.io.IOException;
import wyc.Activator;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyjc.commands.JvmCompile;

/**
 * This class implements an baseline ant task for compiling whiley files via ant
 * and an appropriate build.xml file. The following illustrates how this task
 * can be used in a build.xml file:
 *
 * <pre>
 * <taskdef name="wyjc" classname="wyjc.util.AntTask" classpath="lib/wyjc.jar"/>
 * <wyc whileydir="stdlib" includes="whiley\/**\/*.whiley" excludes="whiley/io/**"/>
 * </pre>
 *
 * <p>
 * The first line defines the new task, and requires <code>wyjc.jar</code> (which
 * contains this class) to be on the classpath; The second invokes the task to
 * compile all files rooted in the <code>stdlib/</code> directory which are in
 * the <code>whiley/</code> package, excluding those in <code>whiley/io</code>.
 * </p>
 *
 * @author David J. Pearce
 *
 */
public class AntTask extends wyc.util.AntTask {

	/**
	 * Construct a new instance of this command.
	 *
	 * @param registry
	 *            The content registry being used to match files to content
	 *            types.
	 * @throws IOException
	 */
	public AntTask() {
		Content.Registry registry = new Activator.Registry();
		this.command = new JvmCompile(registry, Logger.NULL);
	}

	// =======================================================================
	// Configuration
	// =======================================================================

	public void setClassdir(File dir) throws IOException {
		((JvmCompile)command).setClassdir(dir);
	}
}
