// RUN: %target-swift-frontend -disable-experimental-parser-round-trip -enable-experimental-feature BuiltinModule -typecheck -verify %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin

func a(x: Builtin.Int<64>) -> Builtin.Int64 {
    return x
}

func b(x: Builtin.Int<42>) -> Builtin.Int43 {
    return x // expected-error{{}}
}

func c<T>(x: Builtin.Int<T>) {} // expected-error{{}}

// TODO: implement support for dependent-width integer types
func d<let N: Int>(x: Builtin.Int<N>) -> Builtin.Int<N> { // expected-error 2 {{}}
    return x
}
