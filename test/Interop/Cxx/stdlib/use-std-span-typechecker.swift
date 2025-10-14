// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -Xcc -std=c++20 2>&1

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

import StdSpan

let arr: [Int32] = [1, 2, 3]
arr.withUnsafeBufferPointer { ubpointer in
    let _ = ConstSpanOfInt(ubpointer) // okay
    let _ = ConstSpanOfInt(ubpointer.baseAddress!, ubpointer.count) 
    // expected-warning@-1 {{'init(_:_:)' is deprecated: use 'init(_:)' instead.}}
}

arr.withUnsafeBufferPointer { ubpointer in 
    // FIXME: this crashes the compiler once we import span's templated ctors as Swift generics.
    let _ = ConstSpanOfInt(ubpointer.baseAddress, ubpointer.count)
    // expected-error@-1 {{value of optional type 'UnsafePointer<Int32>?' must be unwrapped to a value of type 'UnsafePointer<Int32>'}}
    // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
    // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
}
