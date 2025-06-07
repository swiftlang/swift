// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xcc -fignore-exceptions)
//
// REQUIRES: executable_test

import StdlibUnittest
import MemberInheritance

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

FunctionsTestSuite.test("base member FRT field access") {
  let copyCounter = getCopyCounter().pointee

  let derived = makeCopyTrackedDerivedClass(42)!
  expectEqual(derived.field, 43)
  expectEqual(copyCounter, getCopyCounter().pointee)

  let derivedDerived = makeCopyTrackedDerivedDerivedClass(-5)!
  expectEqual(derivedDerived.field, -4)
  expectEqual(copyCounter, getCopyCounter().pointee)
}

FunctionsTestSuite.test("base member FRT subscript access") {
  let copyCounter = getCopyCounter().pointee

  let derived = makeCopyTrackedDerivedClass(42)!
  expectEqual(derived[1], 44)
  expectEqual(copyCounter, getCopyCounter().pointee)

  let derivedDerived = makeCopyTrackedDerivedDerivedClass(-5)!
  expectEqual(derivedDerived[10], 6)
  expectEqual(copyCounter, getCopyCounter().pointee)
}

FunctionsTestSuite.test("base member FRT subscript accessing reference FRT") {
  let copyCounter = getCopyCounter().pointee

  var base = makeBaseReturningFRTFromSubscript()!
  var frt = base[1]
  expectEqual(frt.getX(), 1)

  let derived = makeDerivedFromBaseReturningFRTFromSubscript()!
  frt = derived[2]
  expectEqual(frt.getX(), 2)
}

FunctionsTestSuite.test("base member FRT subscript accessing reference FRT through constant subscript result value") {
  let derivedNoPtr = makeDerivedFromBaseReturningFRTFromSubscript()!
  var frt = derivedNoPtr[9]
  expectEqual(frt.getX(), 9)

  let derived = makeDerivedFromBaseReturningFRTFromSubscriptPointer()!
  frt = derived[1]!
  expectEqual(frt.getX(), 0)
}

runAllTests()
