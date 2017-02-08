// RUN: %target-typecheck-verify-swift

enum E {
	indirect case Foo(E)
	case Bar
}

var e: E = .Bar
var f: E = .Foo(e)
