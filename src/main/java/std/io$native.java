package std;

import java.math.BigInteger;

import wyjc.runtime.Util;
import wyjc.runtime.WyArray;

public class io$native {
	public static void print(BigInteger value) {
		System.out.print(value);
	}
	public static void println(BigInteger value) {
		System.out.println(value);
	}
	public static void print(WyArray asciiString) {
		Util.print(asciiString);
	}
	public static void println(WyArray asciiString) {
		Util.print(asciiString);
		System.out.println();
	}
}
