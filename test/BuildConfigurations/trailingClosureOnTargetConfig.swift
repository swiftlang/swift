// RUN: %swift -parse %s -verify -D FOO -D BAZ -target x86_64-apple-macosx10.9

#if arch(x86_64) && os(OSX)
	println("Mac") // This should not be parsed as a trailing closure
#endif
