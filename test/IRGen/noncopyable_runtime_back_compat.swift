// RUN: %target-run-simple-swift(-enable-experimental-feature SuppressedAssociatedTypes)
// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// Noncopyable types are suppressed in field metadata in runtimes lacking
// reflection support for them (currently all runtimes). However, we want
// to ensure that noncopyable type metadata is still produced where it is
// "load-bearing" to the generics system, such as in associated types in
// protocol conformances.

protocol Barrable: ~Copyable {
}

struct Bar<T: ~Copyable>: Barrable & ~Copyable {
	var x: Int = 0
}

protocol F: ~Copyable {
	associatedtype B: Barrable & ~Copyable
}

struct Foo: F & ~Copyable {
	typealias B = Bar<Int>
}

func test<T: F & ~Copyable>(_: T.Type) {
	print("\(T.B.self)")
}

// CHECK: Bar<Int>
if #available(SwiftStdlib 5.1, *) {
  test(Foo.self)
} else {
  print("Bar<Int>")
}

class Mirrored {
	var noncopyableField: Bar<Int>

	init() {
		self.noncopyableField = Bar()
	}
}

// CHECK: noncopyableField (0 elements)
if #available(SwiftStdlib 5.1, *) {
  dump(Mirrored())
} else {
  print("noncopyableField (0 elements)")
}
