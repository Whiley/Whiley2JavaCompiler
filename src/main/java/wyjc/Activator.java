package wyjc;

import java.io.*;

import jasm.io.ClassFileReader;
import jasm.io.ClassFileWriter;
import jasm.lang.ClassFile;
import wybs.lang.Build;
import wybs.lang.Build.Project;
import wybs.lang.Build.Task;
import wybs.util.AbstractCompilationUnit.Value;
import wyc.lang.WhileyFile;
import wycc.cfg.Configuration;
import wycc.lang.Module;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.lang.Path.ID;
import wyfs.lang.Content.Type;
import wyfs.util.Trie;
import wyjc.builder.JavaCompileTask;
import wyjc.core.JavaFile;

public class Activator implements Module.Activator {

	// =========================================================================
	// Content Type
	// =========================================================================

	public static final Content.Type<ClassFile> ClassContentType = new Content.Type<ClassFile>() {
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
	// Build Platform
	// =======================================================================

	private static Build.Platform JAVA_PLATFORM = new Build.Platform() {

		@Override
		public String getName() {
			return "java";
		}

		@Override
		public Configuration.Schema getConfigurationSchema() {
			return Configuration.EMPTY_SCHEMA;
		}

		@Override
		public void apply(Configuration configuration) {

		}

		@Override
		public Task initialise(Build.Project project) {
			return new JavaCompileTask(project);
		}

		@Override
		public Type<?> getSourceType() {
			return WhileyFile.BinaryContentType;
		}

		@Override
		public Type<?> getTargetType() {
			return JavaFile.ContentType;
		}

		@Override
		public Content.Filter<?> getSourceFilter() {
			return Content.filter("**", WhileyFile.BinaryContentType);
		}

		@Override
		public Content.Filter<?> getTargetFilter() {
			return Content.filter("**", JavaFile.ContentType);
		}

		@Override
		public Path.Root getSourceRoot(Path.Root root) throws IOException {
			return root.createRelativeRoot(Trie.fromString("bin"));
		}

		@Override
		public Path.Root getTargetRoot(Path.Root root) throws IOException {
			return root.createRelativeRoot(Trie.fromString("bin/java"));
		}

		@Override
		public void execute(Project project, ID path, String name, Value... args) {
			throw new IllegalArgumentException("native Java execution currently unsupported");
		}
	};

	// =======================================================================
	// Start
	// =======================================================================

	@Override
	public Module start(Module.Context context) {
		// Register build platform
		context.register(Build.Platform.class, JAVA_PLATFORM);
		// Register Java Content Types
		context.register(Content.Type.class, JavaFile.ContentType);
		context.register(Content.Type.class, ClassContentType);
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
