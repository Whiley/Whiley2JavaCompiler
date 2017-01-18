package wyjc;

import java.io.*;

import jasm.io.ClassFileReader;
import jasm.io.ClassFileWriter;
import jasm.lang.ClassFile;
import wycc.lang.Command;
import wycc.lang.Module;
import wycc.util.Logger;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyjc.commands.JavaCompile;
import wyjc.commands.JvmCompile;

public class Activator implements Module.Activator {
	// =========================================================================
	// Content Type
	// =========================================================================

	public static final Content.Type<ClassFile> ContentType = new Content.Type<ClassFile>() {
		public Path.Entry<ClassFile> accept(Path.Entry<?> e) {
			if (e.contentType() == this) {
				return (Path.Entry<ClassFile>) e;
			}
			return null;
		}

		@Override
		public ClassFile read(Path.Entry<ClassFile> e, InputStream input)
				throws IOException {
			ClassFileReader reader = new ClassFileReader(input);
			return reader.readClass();
		}

		@Override
		public void write(OutputStream output, ClassFile module)
				throws IOException {
			ClassFileWriter writer = new ClassFileWriter(output);
			writer.write(module);
		}

		@Override
		public String toString() {
			return "Content-Type: class";
		}

		@Override
		public String getSuffix() {
			return "class";
		}
	};

	// =======================================================================
	// Content Registry
	// =======================================================================

	/**
	 * Default implementation of a content registry. This associates whiley and
	 * wyil files with their respective content types.
	 *
	 * @author David J. Pearce
	 *
	 */
	public static class Registry extends wyc.Activator.Registry {
		@Override
		public void associate(Path.Entry e) {
			String suffix = e.suffix();

			if (suffix.equals("class")) {
				e.associate(ContentType, null);
			} else {
				super.associate(e);
			}
		}

		@Override
		public String suffix(Content.Type<?> t) {
			return t.getSuffix();
		}
	}

	/**
	 * The master project content type registry. This is needed for the build
	 * system to determine the content type of files it finds on the file
	 * system.
	 */
	public final Content.Registry registry = new Registry();

	// =======================================================================
	// Start
	// =======================================================================

	@Override
	public Module start(Module.Context context) {
		// FIXME: logger is a hack!
		final Logger logger = new Logger.Default(System.err);
		// List of commands to use
		final Command[] commands = {
				new JvmCompile(registry, logger),
				new JavaCompile(registry, logger)};
		// Register all commands
		for (Command c : commands) {
			context.register(wycc.lang.Command.class, c);
		}
		// Done
		return new Module() {
			// what goes here?
		};
	}

	// =======================================================================
	// Stop
	// =======================================================================

	@Override
	public void stop(Module module, Module.Context context) {
		// could do more here?
	}
}
