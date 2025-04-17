// RUN: %target-swift-frontend -emit-sil -verify %s

final class Bar {
	func update() {
		foo?.baz = Foo2(baz: 5)
	}
	var foo: Foo? = Foo()
}

struct Foo: ~Copyable {
	var baz: Foo2 = Foo2(baz: 0)
}

struct Foo2: ~Copyable {
	var baz: Int = 0
}

