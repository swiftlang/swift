// RUN: %target-swift-frontend -c %s -verify
// REQUIRES: asserts

// TF-1160: Linker error for `@usableFromInline` derivative function but
// non-`@usableFromInline` internal original function.

import _Differentiation

func internalOriginal(_ x: Float) -> Float { x }

@usableFromInline
@derivative(of: internalOriginal)
func usableFromInlineDerivative(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { $0 })
}

// Original error: type-checking passes but TBDGen is not consistent with IRGen.
// <unknown>:0: error: symbol 'AD__$s4main16internalOriginalyS2fF__vjp_src_0_wrt_0' (AD__$s4main16internalOriginalyS2fF__vjp_src_0_wrt_0) is in generated IR file, but not in TBD file
// <unknown>:0: error: please file a radar or open a bug on bugs.swift.org with this code, and add -Xfrontend -validate-tbd-against-ir=none to squash the errors
