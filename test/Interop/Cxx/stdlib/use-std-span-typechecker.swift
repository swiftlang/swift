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
