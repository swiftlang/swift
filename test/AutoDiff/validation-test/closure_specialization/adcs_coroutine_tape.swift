// REQUIRES: executable_test
// RUN: %target-build-swift %s -Onone -o %t.none
// RUN: %target-codesign %t.none
// RUN: %target-run %t.none
// RUN: %target-build-swift %s -O -o %t.opt
// RUN: %target-codesign %t.opt
// RUN: %target-run %t.opt

import _Differentiation

public struct Accumulator: Differentiable {
  public var value: Float
  public init(_ value: Float) { self.value = value }

  public var state: Float {
    get { value }
    set { value = newValue }
    @inline(never) _modify { yield &value }
  }
}

// A non-inlined accessor tapes a coroutine pullback. The branch also tapes
// ordinary multiplication pullbacks, so there is work for ADCS to decline.
@differentiable(reverse, wrt: x)
public func mutateAndBranch(_ x: Float, _ flag: Bool) -> Float {
  var model = Accumulator(x)
  model.state *= x
  if flag { return model.state * x }
  return model.state
}

#if !DEFINITIONS_ONLY
for flag in [false, true] {
  let (value, pullback) = valueWithPullback(at: Float(3), of: {
    mutateAndBranch($0, flag)
  })
  precondition(value == (flag ? 27 : 9))
  precondition(pullback(1) == (flag ? 27 : 6))
  precondition(pullback(2) == (flag ? 54 : 12))
}
#endif
