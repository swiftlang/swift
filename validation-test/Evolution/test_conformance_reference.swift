// RUN: %target-resilience-test --no-symbol-diff
// REQUIRES: executable_test

// Uses swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import conformance_reference


var ConformanceReferenceTest = TestSuite("ConformanceReference")


func useBase<T : BaseProtocol>(_: T) {}

ConformanceReferenceTest.test("BaseConformance") {
  useBase(FirstGeneric<Int>())
  useBase(SecondGeneric<Int>())
}


func useDerived<T : DerivedProtocol>(_: T) {}

ConformanceReferenceTest.test("DerivedConformance") {
  useDerived(SecondGeneric<Int>())
}


protocol EvenMoreDerivedProtocol : DerivedProtocol {}

extension FirstGeneric : EvenMoreDerivedProtocol {}

func useEvenMoreDerived<T : EvenMoreDerivedProtocol>(_ t: T) -> Any.Type {
  return t.getMeAType()
}

ConformanceReferenceTest.test("EvenMoreDerivedConformance") {
  expectTrue(FirstGeneric<Int>.self == useEvenMoreDerived(FirstGeneric<Int>()))
  expectTrue(FirstGeneric<String>.self == useEvenMoreDerived(FirstGeneric<String>()))
}

runAllTests()
