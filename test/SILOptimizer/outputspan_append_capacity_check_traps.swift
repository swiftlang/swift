// RUN: %target-run-simple-swift(-O -enable-experimental-feature Lifetimes)
// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature Lifetimes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

// Regression test for BoundsCheckOpts CheckCapacity merging. Two
// `OutputSpan.append` capacity checks separated by an `@inout` callee that
// fills the span must not be merged using the first call's loaded count.
//
// The bug: `getFixedStorageMergeKey` looks through `load_borrow` for
// CheckCapacity, so two checks against the same `inout` span land in one
// merge group. If `cloneFixedStorageIndex` clones the second check's index
// (a fresh `load` of `_count`) back past the intervening callee, the cloned
// load reads stale memory and the check passes when it should trap.

let suite = TestSuite("OutputSpan bounds check optimization")

@inline(never)
func fillRemaining(_ span: inout OutputSpan<Int>) {
  while !span.isFull {
    span.append(0)
  }
}

@inline(never)
func appendAfterFull(_ span: inout OutputSpan<Int>) {
  span.append(1)
  fillRemaining(&span)
  // After `fillRemaining`, count == capacity. This append must trap.
  span.append(99)
}

suite.test("append after inout callee fills span") {
  expectCrashLater()
  let buffer = UnsafeMutableBufferPointer<Int>.allocate(capacity: 2)
  defer { buffer.deallocate() }
  var span = unsafe OutputSpan(buffer: buffer, initializedCount: 0)
  appendAfterFull(&span)
  expectUnreachable("third append should have trapped on capacity overflow")
  _ = unsafe span.finalize(for: buffer)
}

runAllTests()
