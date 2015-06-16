// This test specifically exercises the interpreter's top-level error handler.
// RUN: not --crash %target-jit-run %s
// REQUIRES: swift_interpreter

// rdar://20809122
enum Error : ErrorType { case Foo }
throw Error.Foo
