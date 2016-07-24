// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation

var NSArrayAPI = TestSuite("NSArrayAPI")

NSArrayAPI.test("mixed types with AnyObject") {
  do {
    let result: AnyObject = [1, "two"]
    let expect: NSArray = [1, "two"]
    expectEqual(expect, result as! NSArray)
  }
  do {
    let result: AnyObject = [1, 2]
    let expect: NSArray = [1, 2]
    expectEqual(expect, result as! NSArray)
  }
}

NSArrayAPI.test("CustomStringConvertible") {
  // FIXME: rdar://problem/27515965 Type checker tries to use the
  // sequence-of-Character initializer here instead of the printing initializer
  // without the 'Any' cast.
  let result = String(NSArray(objects:"A", "B", "C", "D") as Any)
  let expect = "(\n    A,\n    B,\n    C,\n    D\n)"
  expectEqual(expect, result)
}

NSArrayAPI.test("copy construction") {
  let expected = ["A", "B", "C", "D"]
  let x = NSArray(array: expected as NSArray)
  expectEqual(expected, x as! Array)
  let y = NSMutableArray(array: expected as NSArray)
  expectEqual(expected, y as NSArray as! Array)
}

var NSMutableArrayAPI = TestSuite("NSMutableArrayAPI")

NSMutableArrayAPI.test("CustomStringConvertible") {
  // FIXME: rdar://problem/27515965 Type checker tries to use the
  // sequence-of-Character initializer here instead of the printing initializer
  // without the 'Any' cast.
  let result = String(NSMutableArray(objects:"A", "B", "C", "D") as Any)
  let expect = "(\n    A,\n    B,\n    C,\n    D\n)"
  expectEqual(expect, result)
}

runAllTests()
