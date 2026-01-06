// RUN: not %target-swift-frontend -emit-silgen %s

// https://github.com/apple/swift/issues/52464
// Just make sure we don't crash.

func foo(x: inout Int = 0) {}
foo()
