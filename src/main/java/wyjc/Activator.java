package wyjc;

import java.io.*;

import jasm.io.ClassFileReader;
import jasm.io.ClassFileWriter;
import jasm.lang.ClassFile;
import wybs.lang.Build;
import wybs.lang.Build.Graph;
import wybs.lang.Build.Project;
import wybs.lang.Build.Task;
import wybs.lang.CompilationUnit.Name;
import wybs.util.AbstractCompilationUnit.Value;
import wyc.lang.WhileyFile;
import wycc.cfg.Configuration;
import wycc.lang.Module;
import wyfs.lang.Content;
import wyfs.lang.Path;
import wyfs.lang.Path.ID;
import wyfs.lang.Path.Root;
import wyfs.lang.Content.Type;
import wyfs.util.Trie;
import wyil.lang.WyilFile;
import wyil.lang.WyilFile.Decl;
import wyjc.builder.JavaCompileTask;
import wyjc.core.JavaFile;

public class Activator implements Module.Activator {

	private static Trie PKGNAME_CONFIG_OPTION = Trie.fromString("package/name");
	private static Trie DEBUG_CONFIG_OPTION = Trie.fromString("build/js/debug");
	private static Trie TARGET_CONFIG_OPTION = Trie.fromString("build/java/target");
	private static Trie SOURCE_CONFIG_OPTION = Trie.fromString("build/whiley/target");
	private static Value.Bool DEBUG_DEFAULT = new Value.Bool(false);
	private static Value.UTF8 TARGET_DEFAULT = new Value.UTF8("bin/java".getBytes());

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
		private Trie pkg;
		// Specify directory where generated whiley files are found
		private Trie source;
		// Specify directory where generated java files are dumped.
		private Trie target;
		// Specify whether debug mode enabled or not.
		private boolean debug = true;

		@Override
		public String getName() {
			return "java";
		}


		@Override
		public Configuration.Schema getConfigurationSchema() {
			return Configuration.fromArray(
					Configuration.UNBOUND_BOOLEAN(DEBUG_CONFIG_OPTION, "Set debug mode (default is ON)", DEBUG_DEFAULT),
					Configuration.UNBOUND_STRING(TARGET_CONFIG_OPTION, "Specify location for generated JavaScript files", TARGET_DEFAULT));
		}

		@Override
		public void apply(Configuration configuration) {
			this.pkg = Trie.fromString(configuration.get(Value.UTF8.class, PKGNAME_CONFIG_OPTION).unwrap());
			//
			this.debug = configuration.get(Value.Bool.class, DEBUG_CONFIG_OPTION).get();
			this.target = Trie.fromString(configuration.get(Value.UTF8.class, TARGET_CONFIG_OPTION).unwrap());
			this.source = Trie.fromString(configuration.get(Value.UTF8.class, SOURCE_CONFIG_OPTION).unwrap());
		}

		@Override
		public Task initialise(Build.Project project) {
			return new JavaCompileTask(project);
		}

		@Override
		public Type<?> getSourceType() {
			return WyilFile.ContentType;
		}

		@Override
		public Type<?> getTargetType() {
			return JavaFile.ContentType;
		}

		@Override
		public Content.Filter<?> getSourceFilter() {
			return Content.filter("**", WyilFile.ContentType);
		}

		@Override
		public Content.Filter<?> getTargetFilter() {
			return Content.filter("**", JavaFile.ContentType);
		}

		@Override
		public Path.Root getSourceRoot(Path.Root root) throws IOException {
			return root.createRelativeRoot(source);
		}

		@Override
		public Path.Root getTargetRoot(Path.Root root) throws IOException {
			return root.createRelativeRoot(target);
		}

		@Override
		public void execute(Project project, ID path, String name, Value... args) {
			throw new IllegalArgumentException("native Java execution currently unsupported");
		}

		@Override
		public void refresh(Graph graph, Root src, Root bin) throws IOException {
			// Basically, for the pkg wyil we will create a corresponding js file.
			Path.Entry<WyilFile> source = src.get(pkg, WyilFile.ContentType);
			for(Path.Entry<?> file : graph.getParents(source)) {
				Path.Entry<JavaFile> binary = bin.get(file.id(), JavaFile.ContentType);
				// Check whether target binary exists or not
				if (binary == null) {
					// Determine qualified id for module
					Path.ID id = stripFirst(this.source.size(),file.id());
					// Doesn't exist, so create with default value
					binary = bin.create(id, JavaFile.ContentType);
					binary.write(new JavaFile(binary));
				}
				// Register source converted by us into the java file.
				graph.connect(source, binary);
			}
		}

		private Path.ID stripFirst(int n, Path.ID id) {
			return id.subpath(n, id.size());
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
