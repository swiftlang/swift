// RUN: %target-swift-emit-silgen -verify %s
struct Foo {
    var x: String

    var y: Foo {
        borrowing _read {
            yield self
        }
    }
}

struct BigFoo {
    var x: Any

    var y: BigFoo {
        borrowing _read {
            yield self
        }
    }
}
