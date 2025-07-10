// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import InheritedLookup
import StdlibUnittest

var InheritedMemberTestSuite = TestSuite("Test if inherited lookup works")

InheritedMemberTestSuite.test("Regular methods resolve to base classes") {
  // No inheritance (sanity check)
  let one = One()
  expectEqual(one.method(), 1)

  // One level of inheritance
  let iOne = IOne()
  expectEqual(iOne.method(), 1)
  expectEqual(iOne.methodI(), -1)

  // Two levels of inheritance
  let iiOne = IIOne()
  expectEqual(iiOne.method(), 1)
  expectEqual(iiOne.methodI(), -1)
  expectEqual(iiOne.methodII(), -11)

  // Three levels of inheritance
  let iiiOne = IIIOne()
  expectEqual(iiiOne.method(), 1)
  expectEqual(iiiOne.methodI(), -1)
  expectEqual(iiiOne.methodII(), -11)
  expectEqual(iiiOne.methodIII(), -111)
}

InheritedMemberTestSuite.test("Eagerly imported methods resolve to base classes") {
  // No inheritance (sanity check)
  let one = One()
  expectEqual(one[0], 1)

  // One level of inheritance
  let iOne = IOne()
  expectEqual(iOne[0], 1)

  // Two levels of inheritance
  let iiOne = IIOne()
  expectEqual(iiOne[0], 1)

  // Three levels of inheritance
  let iiiOne = IIIOne()
  expectEqual(iiiOne[0], 1)
}

runAllTests()
