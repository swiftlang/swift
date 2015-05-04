// RUN: not %target-jit-run %s

// rdar://20809122
enum Error : _ErrorType { case Foo }
throw Error.Foo
