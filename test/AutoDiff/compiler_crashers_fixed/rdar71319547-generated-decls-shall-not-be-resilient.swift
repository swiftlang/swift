// RUN: %target-build-swift -enable-library-evolution %s
// RUN: %target-build-swift -O -enable-library-evolution %s
// RUN: %target-build-swift -O -g -enable-library-evolution %s

// rdar://71319547

import _Differentiation

@differentiable(reverse, wrt: x)
public func i_have_a_pullback_struct(_ x: Float) -> Float {
  return x
}

@differentiable(reverse, wrt: x)
public func i_have_a_branching_trace_enum(_ x: Float) -> Float {
  if true {
    return x
  } else {
    return x.squareRoot()
  }
}
