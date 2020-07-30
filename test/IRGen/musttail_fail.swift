// RUN: not --crash %target-swift-frontend %s -emit-ir

class Klass {}

// NOTE: We expect this test to fail since sil standardizes with a single epilogue block
// which currently thwarts llvm level pattern matching the must tail call immediately followed by return.
// Support for more complicated CFGs will be implemented in a subsequent patch.
// We just want to make sure that LLVM asserts here as we expect it to without that support.

@_semantics("optimize.sil.tail_always")
func mustTailRecurse(_ x: Int) -> Int {
  @_semantics("optimize.sil.tail_always")
  func mustTailReturn(_ x: Int) -> Int {
    return x
  }

  if x > 0 {
    return mustTailRecurse(x - 1)
  }

  return mustTailReturn(x)
}


mustTailRecurse(2)