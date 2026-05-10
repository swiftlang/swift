// RUN: %target-swift-frontend -disable-experimental-parser-round-trip -disable-availability-checking -enable-experimental-feature BuiltinModule -typecheck -verify %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

func a<let N: Int, T>(x: Builtin.FixedArray<N, T>) -> Builtin.FixedArray<N, T> {
    return x
}

func b<T, let N: Int>(x: Builtin.FixedArray<T, N>) -> Builtin.FixedArray<T, N> { // expected-error 2 {{}}
    return x
}

func c(x: Builtin.FixedArray<4, Int>) {}

func d(x: Builtin.FixedArray<4, 4>) {} // expected-error{{}}
func e(x: Builtin.FixedArray<Int, Int>) {} // expected-error{{}}
func f(x: Builtin.FixedArray<Int, 4>) {} // expected-error{{}}

struct CopyableContainingNoncopyableVector<T: ~Copyable>: Copyable {
    var x: Builtin.FixedArray<4, T> // expected-error{{}}
}

struct CopyableContainingCopyableVector<T>: Copyable {
    var x: Builtin.FixedArray<4, T>
}

struct MyVector<let N: Int, T: ~Copyable>: ~Copyable {
    var storage: Builtin.FixedArray<N, T>
}
extension MyVector: Copyable where T: Copyable {}
extension MyVector: BitwiseCopyable where T: BitwiseCopyable {}

struct BitwiseCopyableVector<T: BitwiseCopyable>: BitwiseCopyable {
    var x: Builtin.FixedArray<4, T>
}

struct NonBitwiseCopyableVector<T>: BitwiseCopyable {
    var x: Builtin.FixedArray<4, T> // expected-error{{}}
}

