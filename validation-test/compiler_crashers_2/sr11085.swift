// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts

func foo(x: Int) {}
func foo(line: String = #line) {}
foo()
