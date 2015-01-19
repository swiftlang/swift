// RUN: %target-parse-verify-swift

// FIXME: When we can IRGen recursively specified enums, remove the '-parse' flag from this test
enum E {
	case Foo(E)
	case Bar
}

var e: E = .Bar
var f: E = .Foo(e)
