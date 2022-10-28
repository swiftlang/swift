// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import CustomSequence
import Cxx

var CxxCollectionTestSuite = TestSuite("CxxCollection")

CxxCollectionTestSuite.test("SimpleCollectionNoSubscript as Swift.Collection") {
  let c = SimpleCollectionNoSubscript()
  expectEqual(c.first, 1)
  expectEqual(c.last, 5)
}

CxxCollectionTestSuite.test("SimpleCollectionReadOnly as Swift.Collection") {
  let c = SimpleCollectionReadOnly()
  expectEqual(c.first, 1)
  expectEqual(c.last, 5)
}

CxxCollectionTestSuite.test("SimpleArrayWrapper as Swift.Collection") {
  let c = SimpleArrayWrapper()
  expectEqual(c.first, 10)
  expectEqual(c.last, 50)
}

runAllTests()
