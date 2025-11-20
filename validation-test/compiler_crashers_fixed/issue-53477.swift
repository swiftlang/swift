// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/53477

func foo(x: Int) {}
func foo(line: String = #line) {}
foo()
