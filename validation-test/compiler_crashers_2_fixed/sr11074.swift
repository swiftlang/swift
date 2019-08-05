// RUN: not %target-swift-frontend -typecheck %s

func foo(x: Int) {}
func foo(line: String = #line) {}
foo()
