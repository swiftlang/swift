// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test
//
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import StdPair
import CxxStdlib
import Cxx

var StdPairTestSuite = TestSuite("StdPair")

StdPairTestSuite.test("StdPair.elements") {
  var pi = getIntPair()
  expectEqual(pi.first, -5)
  expectEqual(pi.second, 12)
  pi.first = 11
  expectEqual(pi.first, 11)
  expectEqual(pi.second, 12)
}

StdPairTestSuite.test("StdPair.ref.elements") {
  let pi = getIntPairPointer().pointee
  expectEqual(pi.first, 4)
  expectEqual(pi.second, 9)
}

StdPairTestSuite.test("PairStructInt.elements") {
  let pair = getPairStructInt(11)
  expectEqual(pair.first.x, 22)
  expectEqual(pair.first.y, -11)
  expectEqual(pair.second, 11)
}

StdPairTestSuite.test("StdPair as CxxPair") {
  func changeFirst(_ p: inout any CxxPair<CInt, CInt>) {
    p.first = 123
  }

  var pair: any CxxPair<CInt, CInt> = getIntPair()
  changeFirst(&pair)
  expectEqual(pair.first, 123)
}

runAllTests()
