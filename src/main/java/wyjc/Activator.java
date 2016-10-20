package wyjc;

import java.io.*;

import jasm.io.ClassFileReader;
import jasm.io.ClassFileWriter;
import jasm.lang.ClassFile;
import wyfs.lang.Content;
import wyfs.lang.Path;

public class Activator {
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
}
