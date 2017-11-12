// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var UnfoldTests = TestSuite("UnfoldSequence")

UnfoldTests.test("sequence(state:next:)") {
  // FIXME: The full type signatures on these closures should not be
  // necessary, but at the moment the compiler gives very confusing errors if
  // we don't have them.

  let s0 = sequence(state: 1, next: { (val: inout Int) -> Int? in
    defer { val *= 2 }; return val > 16 ? nil : val
  })
  checkSequence([1,2,4,8,16], s0)

  let s1 = sequence(state: (), next: { (_: inout ()) -> Int? in 1 })
  checkSequence([1, 1, 1], s1.prefix(3))

  let s2 = sequence(state: (1..<6).makeIterator(), next: {
    (iter: inout CountableRange<Int>.Iterator) in
    iter.next()
  })
  checkSequence(1..<6, s2)

  // Make sure we don't evaluate any step in advance
  var calls = 0
  var s3 = sequence(state: 0, next: { (val: inout Int) -> Int? in
    calls += 1; val += 1; return val
  })
  for i in 1..<6 {
    expectEqual(i, s3.next())
    expectEqual(i, calls)
  }

  // Make sure we don't invoke next() after it returns nil
  calls = 0
  var s4 = sequence(state: 1, next : { (val: inout Int) -> Int? in
    calls += 1; defer { val *= 2 }; return val > 16 ? nil : val
  })
  checkSequence([1,2,4,8,16], s4)
  expectEqual(6, calls)

  let s5 = sequence(state: (), next: { (_: inout ()) -> Int? in nil })
  checkSequence([], s5)

  // This is similar to s0 except calling the `next` closure after it returns
  // nil starts returning values again.
  let s6 = sequence(state: 1, next: { (val: inout Int) -> Int? in
    defer { val *= 2 }
    return val == 32 ? nil : val
  })
  checkSequence([1,2,4,8,16], s6)
}

UnfoldTests.test("sequence(first:next:)") {
  let s0 = sequence(first: 1, next: { $0 < 50 ? $0 * 2 : nil })
  expectEqualSequence([1, 2, 4, 8, 16, 32, 64], s0)

  // Make sure we don't evaluate any step in advance
  var calls = 0
  var s1 = sequence(first: 0, next: { calls += 1; return $0 + 1 })
  for i in 0..<5 {
    expectEqual(i, s1.next())
    expectEqual(i, calls)
  }

  // Make sure we don't invoke next() after it returns nil
  calls = 0
  let s2 = sequence(first: 1, next: {
    calls += 1
    return $0 >= 16 ? nil : $0 * 2
  })
  checkSequence([1,2,4,8,16], s2)
  expectEqual(5, calls)
}

runAllTests()
