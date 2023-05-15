// RUN: %target-swift-frontend -typecheck -verify %s

// Tuples with noncopyable elements are not yet supported. Make sure we reject
// them when code attempts to form such a type explicitly or by inference.

@_moveOnly struct Butt {
    var x: Int
}

@_moveOnly
struct Foo {
    var t: (Int, Butt) // expected-error{{tuples with noncopyable elements are not supported}}
}
@_moveOnly
struct Bar<T> {
    var t: (T, Butt) // expected-error{{tuples with noncopyable elements are not supported}}
    var u: (Int, (T, Butt)) // expected-error{{tuples with noncopyable elements are not supported}}
}

func inferredTuples<T>(x: Int, y: borrowing Butt, z: T) {
    let a = (x, y) // expected-error{{tuples with noncopyable elements are not supported}}
    let b = (y, z) // expected-error{{tuples with noncopyable elements are not supported}}
    let c = (x, y, z) // expected-error{{tuples with noncopyable elements are not supported}}
    _ = a
    _ = b
    _ = c
}
