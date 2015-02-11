// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var NSArrayAPI = TestSuite("NSArrayAPI")

NSArrayAPI.test("mixed types with AnyObject") {
  if true {
    let result: AnyObject = [1, "two"]
    let expect: NSArray = [1, "two"]
    expectEqual(expect, result as! NSArray)
  }
  if true {
    let result: AnyObject = [1, 2]
    let expect: NSArray = [1, 2]
    expectEqual(expect, result as! NSArray)
  }
}

NSArrayAPI.test("Printable") {
  let result = toString(NSArray(objects:"A", "B", "C", "D"))
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

NSMutableArrayAPI.test("Printable") {
  let result = toString(NSMutableArray(objects:"A", "B", "C", "D"))
  let expect = "(\n    A,\n    B,\n    C,\n    D\n)"
  expectEqual(expect, result)
}

runAllTests()
