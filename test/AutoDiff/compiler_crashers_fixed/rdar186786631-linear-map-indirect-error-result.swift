// RUN: %target-swift-frontend -emit-sil -verify %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -O %s -o /dev/null

// rdar://186786631
// Ensure the indirect error result of a typed-throws callee is accounted for
// when mapping a parameter index to a SIL argument index. The callee below has
// an address-only error type, so it carries an `@error_indirect` result, which
// occupies a SIL argument slot ahead of the parameters.

import _Differentiation

struct BoxError: Error {
  // `Any` makes `BoxError` address-only.
  var payload: Any
}

func mayFail(_ x: Double) throws(BoxError) -> Double {
  if x.isNaN {
    throw BoxError(payload: x)
  }
  return x * x
}

@differentiable(reverse)
func caller(_ x: Double) -> Double {
  do {
    return try mayFail(x)
  } catch {
    return 0.0
  }
}

@inline(never)
func gradientOfCaller(_ x: Double) -> Double {
  gradient(at: x, of: caller)
}
