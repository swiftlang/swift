// RUN: %target-swift-frontend -emit-sil -verify %s

func test(foo: borrowing Foo, s: String) -> Bool {
	return foo.children[0].name == s
}

func test2(foo: borrowing Foo) -> String {
	return foo.children[0].name
}

struct Foo: ~Copyable {
	var children: Bar<Foo>
	var name: String
}

struct Bar<T: ~Copyable>: ~Copyable {
	subscript(x: Int) -> T {
		_read {
			fatalError()
		}
	}
}

