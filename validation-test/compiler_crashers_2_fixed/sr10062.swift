// RUN: not %target-swift-frontend -emit-silgen %s

// Just make sure we don't crash.

func foo(x: inout Int = 0) {}
foo()
