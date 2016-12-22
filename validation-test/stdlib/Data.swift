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

DataTestSuite.test("associated types") {
  typealias Subject = Data
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Subject.self,
    iteratorType: Data.Iterator.self,
    subSequenceType: Subject.self,
    indexType: Int.self,
    indexDistanceType: Int.self,
    indicesType: CountableRange<Int>.self)
}

DataTestSuite.test("Data SubSequence") {
  let array: [UInt8] = [0, 1, 2, 3, 4, 5, 6, 7]
  var data = Data(bytes: array)

  // FIXME: Iteration over Data slices is currently broken:
  // [SR-4292] Foundation.Data.copyBytes is zero-based.
  //           Data.Iterator assumes it is not.
  // checkRandomAccessCollection(array, data)

  for i in 0..<data.count {
    for j in i..<data.count {
      var dataSlice = data[i..<j]
      let arraySlice = array[i..<j]
      if dataSlice.count > 0 {
        expectEqual(dataSlice.startIndex, i)
        expectEqual(dataSlice.endIndex, j)
        
        expectEqual(dataSlice[i], arraySlice[i])

        dataSlice[i] = 0xFF
        
        expectEqual(dataSlice.startIndex, i)
        expectEqual(dataSlice.endIndex, j)
      }
    }
  }
}

runAllTests()
