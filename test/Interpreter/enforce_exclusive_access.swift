// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -enforce-exclusivity=checked -Onone
//
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

// Tests for traps at run time when enforcing exclusive access.

import StdlibUnittest
import SwiftPrivatePthreadExtras

struct X {
  var i = 7
}

/// Calling this function will begin a read access to the variable referred to
/// in the first parameter that lasts for the duration of the call. Any
/// accesses in the closure will therefore be nested inside the outer read.
func readAndPerform<T>(_ _: UnsafePointer<T>, closure: () ->()) {
  closure()
}

/// Begin a modify access to the first paraemter and call the closure inside it.
func modifyAndPerform<T>(_ _: UnsafeMutablePointer<T>, closure: () ->()) {
  closure()
}

var globalX = X()

var ExclusiveAccessTestSuite = TestSuite("ExclusiveAccess")

ExclusiveAccessTestSuite.test("Read") {
  let l = globalX // no-trap
  _blackHole(l)
}

// It is safe for a read access to overlap with a read.
ExclusiveAccessTestSuite.test("ReadInsideRead") {
  readAndPerform(&globalX) {
    let l = globalX // no-trap
    _blackHole(l)
  }
}

ExclusiveAccessTestSuite.test("ModifyInsideRead")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("modify/read access conflict detected on address")
  .code
{
  readAndPerform(&globalX) {
    expectCrashLater()
    globalX = X()
  }
}

ExclusiveAccessTestSuite.test("ReadInsideModify")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("read/modify access conflict detected on address")
  .code
{
  modifyAndPerform(&globalX) {
    expectCrashLater()
    let l = globalX
    _blackHole(l)
  }
}

ExclusiveAccessTestSuite.test("ModifyInsideModify")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("modify/modify access conflict detected on address")
  .code
{
  modifyAndPerform(&globalX) {
    expectCrashLater()
    globalX.i = 12
  }
}

var globalOtherX = X()

// It is safe for two modifications of different variables
// to overlap.
ExclusiveAccessTestSuite.test("ModifyInsideModifyOfOther") {
  modifyAndPerform(&globalOtherX) {
    globalX.i = 12 // no-trap
  }
}

// The access durations for these two modifications do not overlap
ExclusiveAccessTestSuite.test("ModifyFollowedByModify") {
  globalX = X()
  _blackHole(())

  globalX = X() // no-trap
}

ExclusiveAccessTestSuite.test("ClosureCaptureModifyModify") {
  var x = X()
  modifyAndPerform(&x) {
    // FIXME: This should be caught dynamically.
    x.i = 12
  }
}

// Test for per-thread enforcement. Don't trap when two different threads
// have overlapping accesses
ExclusiveAccessTestSuite.test("PerThreadEnforcement") {
  modifyAndPerform(&globalX) {
    var (_, otherThread) = _stdlib_pthread_create_block(nil, { (_ : Void) -> () in
      globalX.i = 12 // no-trap
      return ()
    }, ())

    _ = _stdlib_pthread_join(otherThread!, Void.self)
  }
}



runAllTests()
