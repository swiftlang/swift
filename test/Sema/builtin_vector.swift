// RUN: %target-swift-frontend -disable-availability-checking -enable-experimental-feature ValueGenerics -enable-experimental-feature BuiltinModule -typecheck -verify %s

import Builtin

func a<let N: Int, T>(x: Builtin.Vector<N, T>) -> Builtin.Vector<N, T> {
    return x
}

func b<T, let N: Int>(x: Builtin.Vector<T, N>) -> Builtin.Vector<T, N> { // expected-error 2 {{}}
    return x
}

func c(x: Builtin.Vector<4, Int>) {}

func d(x: Builtin.Vector<4, 4>) {} // expected-error{{}}
func e(x: Builtin.Vector<Int, Int>) {} // expected-error{{}}
func f(x: Builtin.Vector<Int, 4>) {} // expected-error{{}}

struct CopyableContainingNoncopyableVector<T: ~Copyable>: Copyable {
    var x: Builtin.Vector<4, T> // expected-error{{}}
}

struct CopyableContainingCopyableVector<T>: Copyable {
    var x: Builtin.Vector<4, T>
}

struct MyVector<let N: Int, T: ~Copyable>: ~Copyable {
    var storage: Builtin.Vector<N, T>
}
extension MyVector: Copyable where T: Copyable {}
extension MyVector: BitwiseCopyable where T: BitwiseCopyable {}

struct BitwiseCopyableVector<T: BitwiseCopyable>: BitwiseCopyable {
    var x: Builtin.Vector<4, T>
}

struct NonBitwiseCopyableVector<T>: BitwiseCopyable {
    var x: Builtin.Vector<4, T> // expected-error{{}}
}

