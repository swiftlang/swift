// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -cxx-interop-static-array-as-collection)
//
// REQUIRES: executable_test

import StdlibUnittest
import StaticArrayCollection

var StaticArrayCollectionTestSuite = TestSuite("StaticArrayCollection")

StaticArrayCollectionTestSuite.test("HasIntArray.subscript") {
  let arr = HasIntArray()
  expectEqual(arr.array[0], 1)
}

StaticArrayCollectionTestSuite.test("HasIntArray.for-in") {
  let arr = HasIntArray()
  var i = CInt(1)
  for e in arr.array {
    expectEqual(e, i)
    i += 1
  }
}

StaticArrayCollectionTestSuite.test("HasIntArray.enumerated()") {
  let arr = HasIntArray()
  for (n, e) in arr.array.enumerated() {
    expectEqual(CInt(n + 1), e)
  }
}

StaticArrayCollectionTestSuite.test("HasIntArray.count") {
  let arr = HasIntArray()
  expectEqual(arr.array.count, 8)
}

StaticArrayCollectionTestSuite.test("HasIntArray.withInnerStorage") {
  let arr = HasIntArray()
  arr.array.withInnerStorage {
    expectEqual($0.pointee, 1)
  }
}

runAllTests()

