// RUN: %target-build-swift -O %s

// https://github.com/apple/swift/issues/54935
// SIL verification error regarding substituted function types and
// `differentiable_function_extract` instruction. Occurs only with `-O`.

// FIXME(https://github.com/apple/swift/issues/55466): Disabled due to flakiness on Linux , likely related to TF-1197.
// XFAIL: *

import _Differentiation

func exampleVJP_1(_ x0: Float) -> (Float, (Float) -> (Float)) {
  (
    x0 * x0,
    { (2 * x0 * $0) }
  )
}

func bar() {
  let f = differentiableFunction(from: exampleVJP_1)
  let pb = pullback(at: 10, of: f)
  _ = pb(1)
}
