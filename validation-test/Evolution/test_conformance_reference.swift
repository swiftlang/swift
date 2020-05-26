// RUN: %target-resilience-test --no-symbol-diff
// REQUIRES: executable_test

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
