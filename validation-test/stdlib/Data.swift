// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import StdlibCollectionUnittest
import Foundation

var DataTestSuite = TestSuite("Data")
DataTestSuite.test("Data.Iterator semantics") {
  // Empty data
  checkSequence([], Data())

  // Small data
  checkSequence([1,2,4,8,16], Data(bytes: [1,2,4,8,16]))

  // Boundary conditions
  checkSequence([5], Data(bytes: [5]))
  checkSequence(1...31, Data(bytes: Array(1...31)))
  checkSequence(1...32, Data(bytes: Array(1...32)))
  checkSequence(1...33, Data(bytes: Array(1...33)))

  // Large data
  var data = Data(count: 65535)
  data.withUnsafeMutableBytes { (ptr: UnsafeMutablePointer<UInt8>) -> () in
    for i in 0..<data.count {
      ptr[i] = UInt8(i % 23)
    }
  }
  checkSequence((0..<65535).lazy.map({ UInt8($0 % 23) }), data)
}
runAllTests()
