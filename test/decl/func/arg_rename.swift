// RUN: %swift -parse %s -verify

// Renaming of arguments.
func foo(a x:Int, b y: Int) { }
foo(a: 5, b: 7)

