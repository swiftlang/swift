// RUN: %target-swift-frontend -emit-sil -verify %s

// rdar://142509673

struct Foo: ~Copyable {
    var storage: UnsafeRawPointer
}

class Bar {
    class var foo: Foo { fatalError() }
}

func test(foo: borrowing Foo) -> Bool {
    fatalError()
}

func test2(bar: Bar.Type) -> Bool {
    guard test(foo: bar.foo) else {
        return false
    }
    return true
}
