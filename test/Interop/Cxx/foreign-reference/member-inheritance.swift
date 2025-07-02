// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-g -I %S/Inputs -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import StdlibUnittest
import MemberInheritance

var FunctionsTestSuite = TestSuite("Calling functions in foreign reference classes")

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

if #available(SwiftStdlib 5.8, *) {
  FunctionsTestSuite.test("virtual members in FRT") {
    let i = Immortal.create()
    expectEqual(42, i.get42())
    expectEqual(0, i.getIntValue())

    let base = castToImmortalBase(i)
    expectEqual(42, base.get42())
    expectEqual(42, base.getOverridden42())
    expectEqual(0, base.getIntValue())

    i.setIntValue(566)
    expectEqual(566, i.getIntValue())
    expectEqual(566, base.getIntValue())

    let d = DerivedFromImmortal.create()
    expectEqual(42, d.get42())
    expectEqual(42, d.getOverridden42())
    d.setIntValue(321)
    expectEqual(321, d.getIntValue())
    let base2 = castToImmortalBase(castToImmortal(d))
    expectEqual(321, base2.getIntValue())
  }
}

#if !os(Windows) 
// FIXME in Windows, non-trivial C++ class with trivial ABI is not yet available in Swift
FunctionsTestSuite.test("C++ virtual method with complex parameter") {
  @available(SwiftStdlib 5.8, *)
  func f(simpleClass: HasDestructor, immortalClass: Immortal2) {
    immortalClass.virtualMethod(simpleClass)
  }
}
#endif

if #available(SwiftStdlib 5.8, *) {
  FunctionsTestSuite.test("renamed C++ virtual method") {
    func f(immortalClass: Immortal2) {
      immortalClass.swiftVirtualRename()
    }
  } 
}

runAllTests()
