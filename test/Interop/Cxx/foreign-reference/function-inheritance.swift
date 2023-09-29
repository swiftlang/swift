// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import FunctionInheritance

var FunctionsTestSuite = TestSuite("Calling functions in base foreign reference classes")

FunctionsTestSuite.test("base member FRT calls do not require copying") {
  let derived = makeCopyTrackedDerivedClass(42)!
  var copyCounter = getCopyCounter().pointee
  expectEqual(derived.getX(), 42)
  expectEqual(copyCounter, getCopyCounter().pointee)
  expectEqual(derived.getDerivedX(), 42)
  expectEqual(copyCounter, getCopyCounter().pointee)

  let derivedDerived = makeCopyTrackedDerivedDerivedClass(-5)!
  copyCounter = getCopyCounter().pointee
  expectEqual(derivedDerived.getX(), -5)
  expectEqual(derivedDerived.getY(), 11)
  expectEqual(copyCounter, getCopyCounter().pointee)
}

runAllTests()
