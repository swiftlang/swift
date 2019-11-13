// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_change_stored_to_observed


var ChangeStoredToObservedTest = TestSuite("ChangeStoredToObserved")

ChangeStoredToObservedTest.test("ChangeStoredToObserved") {
  let t = ChangeStoredToObserved()

  t.friend = "dog"
  t.friend = "pony"
  t.friend = "chicken"

  if getVersion() == 0 {
    expectEqual([], t.friends)
    expectEqual(0, t.count)
  } else {
    expectEqual(["cat", "dog", "pony"], t.friends)
    expectEqual(3, t.count)
  }
}

runAllTests()
