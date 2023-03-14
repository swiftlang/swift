// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
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
  var pi = getIntPair().pointee
  expectEqual(pi.first, -5)
  expectEqual(pi.second, 12)
  pi.first = 11
  expectEqual(pi.first, 11)
  expectEqual(pi.second, 12)
}

runAllTests()
