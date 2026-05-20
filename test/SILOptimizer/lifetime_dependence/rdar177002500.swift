// RUN: %target-swift-frontend -emit-sil -O -enable-experimental-feature Lifetimes %s

// REQUIRES: swift_feature_Lifetimes

// Minimal reproducer for SIL optimizer crash: rdar://177002500

func foo(x: Int) {
    bar { (_: consuming A<Void>) -> B in
        _ = x
        fatalError()
    }
}

func bar<T>(_: @_lifetime(copy a) (_ a: consuming A<T>) -> B) {}

struct A<T>: ~Escapable {}
struct B: ~Escapable {}
