// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//    * Neither the name of the <organization> nor the
//      names of its contributors may be used to endorse or promote products
//      derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL DAVID J. PEARCE BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package std;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.math.BigInteger;
import java.util.HashMap;

import wyjc.runtime.Util;
import wyjc.runtime.WyByte;
import wyjc.runtime.WyLambda;
import wyjc.runtime.WyObject;
import wyjc.runtime.WyArray;
import wyjc.runtime.WyRecord;

public class fs$native {
	public static WyRecord open(WyArray _filename) {
		String filename = Util.il2str(_filename);
		RandomAccessFile file;
		try {
			// FIXME: should extend fs.open to accept MODE
			file = new RandomAccessFile(filename, "rw");
			WyRecord rec = new WyRecord();
			rec.put("readAll", readAll(file));
			rec.put("read", read(file));
			rec.put("write", write(file));
			rec.put("flush", flush(file));
			rec.put("hasMore", hasMore(file));
			rec.put("close", close(file));
			rec.put("available", available(file));
			return rec;
		} catch (FileNotFoundException e) {
			// FIXME: how should this be handled?
			throw new RuntimeException(e);
		}
	}

	private static final int CHUNK_SIZE = 1024;

	private static WyLambda readAll(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				WyArray r = new WyArray();
				try {
					int nbytes = 0;
					do {
						byte[] bytes = new byte[CHUNK_SIZE];
						nbytes = file.read(bytes);
						for(int i=0;i!=nbytes;++i) {
							r.add(WyByte.valueOf(bytes[i]));
						}
					} while(nbytes == CHUNK_SIZE);
				} catch (IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}

				return r;
			}

		};
	}


	private static WyLambda read(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				int size = (Integer) parameters[0];
				byte[] data = new byte[size];
				try {
					int read = file.read(data);
					return toByteArray(data,read);
				} catch (IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}
			}

		};
	}


	private static WyLambda write(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				WyArray bytes = (WyArray) parameters[0];
				try {
					file.write(fromByteArray(bytes));
					return BigInteger.valueOf(bytes.size());
				} catch (IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}
			}
		};
	}

	private static WyLambda flush(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				try {
					file.close();
					return null;
				} catch(IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}
			}
		};
	}

	private static WyLambda hasMore(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				try {
					return file.length() < file.getFilePointer();
				} catch (IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}
			}

		};
	}

	private static WyLambda close(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				try {
					file.close();
					return null;
				} catch (IOException e) {
					// FIXME: how to handle this?
					throw new RuntimeException(e);
				}
			}

		};
	}

	private static WyLambda available(final RandomAccessFile file) {
		return new WyLambda() {

			@Override
			public Object call(Object[] parameters) {
				return BigInteger.ONE;
			}

		};
	}

	private static byte[] fromByteArray(WyArray bytes) {
		byte[] r = new byte[bytes.size()];
		for (int i = 0; i != r.length; ++i) {
			r[i] = ((WyByte) bytes.get(i)).value();
		}
		return r;
	}

	private static WyArray toByteArray(byte[] bytes, int len) {
		WyArray r = new WyArray();
		for (int i = 0; i != len; ++i) {
			r.add(WyByte.valueOf(bytes[i]));
		}
		return r;
	}
}
