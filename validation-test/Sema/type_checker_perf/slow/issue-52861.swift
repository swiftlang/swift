// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/52861

// This type checks in ~800ms with the default limits, but we make it fail sooner.

import simd

func test(_ p0: float2, _ p1: float2, _ p2: float2, _ p3: float2) {
    let i = 3*(-p0 + 3*p1) - (3*p2 + p3) // expected-error{{reasonable time}}
    let j = 6*(p0 - 2*p1 + p2) // expected-error{{reasonable time}}
    let k = 3*(p1 - p0)
    print(i, j, k)
}
test(float2(0.1, 1.2), float2(2.3, 3.4), float2(4.5, 5.6), float2(6.7, 7.8))
