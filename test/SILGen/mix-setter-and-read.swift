// RUN: %target-swift-emit-silgen -verify %s

struct Attributt<T> {
    public var read_and_set: T {
        _read {
            yield UnsafePointer<T>(bitPattern: 17)!.pointee
        }
        nonmutating set {
        }
    }
    public var address_and_set: T {
        unsafeAddress {
            return UnsafePointer<T>(bitPattern: 38)!
        }
        nonmutating set {
        }
    }
}

func foo(x: inout (Int) -> Int) { }

func bar(x: Attributt<(Int) -> Int>) {
    foo(x: &x.read_and_set)
    foo(x: &x.address_and_set)
}

