// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation

var tests = TestSuite("NSDictionary")

tests.test("copy construction") {
  let expected = ["A":1, "B":2, "C":3, "D":4]
  let x = NSDictionary(dictionary: expected as NSDictionary)
  expectEqual(expected, x as! Dictionary)
  let y = NSMutableDictionary(dictionary: expected as NSDictionary)
  expectEqual(expected, y as NSDictionary as! Dictionary)
}
// rdar://problem/27875914
tests.test("subscript with Any") {
  let d = NSMutableDictionary()
  d["k"] = "@this is how the world ends"
  expectEqual((d["k"]! as AnyObject).characterAtIndex(0), 0x40)
  d["k"] = nil
  expectTrue(d["k"] == nil)
}

runAllTests()
