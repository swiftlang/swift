// This test specifically exercises the interpreter's top-level error handler.
// RUN: not %target-jit-run %s
// REQUIRES: swift_interpreter

// rdar://20809122
enum Error : _ErrorType { case Foo }
throw Error.Foo
