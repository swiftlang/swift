// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// REQUIRES: executable_test

import InheritFRT
import StdlibUnittest

var InheritFRTTests = TestSuite("Retain and release inherited FRTs without crashing")

// Simulate some copy operations that cause retain/releases
@inline(never)
func use<T: Copyable>(_ t: T) -> (T, T) { return (t, t) }

InheritFRTTests.test("Simple: simple record types without inheritance") {
  let _ = use(makeSimpleValue())
  let _ = use(makeSimpleShared())
  let _ = use(makeSimpleImmortal())
}

InheritFRTTests.test("SingleShared: single inheritance (one level) with a shared refence base") {
  let _ = use(makeSingleShared_Shared())
  let _ = use(makeSingleShared_NoAttr())
  let _ = use(makeSingleShared_Shared_Final())
  let _ = use(makeSingleShared_NoAttr_Final())
}

InheritFRTTests.test("SingleShared: single inheritance (two levels) with a shared refence base") {
  let _ = use(makeSingleShared_Shared_Shared())
  let _ = use(makeSingleShared_Shared_NoAttr())
  let _ = use(makeSingleShared_NoAttr_Shared())
  let _ = use(makeSingleShared_NoAttr_NoAttr())
}

InheritFRTTests.test("SingleImmortal: single inheritance (one level) with an immortal refence base") {
  let _ = use(makeSingleImmortal_Immort())
  let _ = use(makeSingleImmortal_NoAttr())
  let _ = use(makeSingleImmortal_Immort_Final())
  let _ = use(makeSingleImmortal_NoAttr_Final())
}

InheritFRTTests.test("SingleImmortal: single inheritance (two levels) with an immortal refence base") {
  let _ = use(makeSingleImmortal_Immort_Immort())
  let _ = use(makeSingleImmortal_Immort_NoAttr())
  let _ = use(makeSingleImmortal_NoAttr_Immort())
  let _ = use(makeSingleImmortal_NoAttr_NoAttr())
}

InheritFRTTests.test("OverloadShared: single inheritance with overloaded retain/release ops") {
  let _ = use(makeOverloadShared_Shared())
  let _ = use(makeOverloadShared_Shared_Shared())
  let _ = use(makeOverloadShared_Shared_NoAttr())
}

InheritFRTTests.test("OneShared - multiple inheritance with one shared reference base") {
  let _ = use(makeOneShared_RU_Shared())
  let _ = use(makeOneShared_UR_Shared())
  let _ = use(makeOneShared_RU_NoAttr())
  let _ = use(makeOneShared_UR_NoAttr())
  let _ = use(makeOneShared_DRU_Shared())
  let _ = use(makeOneShared_UDR_Shared())
  let _ = use(makeOneShared_DRU_NoAttr())
  let _ = use(makeOneShared_UDR_NoAttr())
}

InheritFRTTests.test("TwoShared: inheriting two shared reference bases") {
  let _ = use(makeTwoShared_NoAttr())
  let _ = use(makeTwoShared_Shared())
  let _ = use(makeTwoShared_Shared_UsingA())
  let _ = use(makeTwoShared_Shared_UsingB())
}

InheritFRTTests.test("DiamondRef: inheriting same ref base twice due to diamond inheritance") {
  let _ = use(makeDiamondRef_NoAttr())
  let _ = use(makeDiamondRef_Shared())
  // The following cause the compiler to crash:
  // let _ = use(makeDiamondRef_VV_NoAttr())
  // let _ = use(makeDiamondRef_VV_Shared())
  // let _ = use(makeDiamondRef_XV_NoAttr())
  // let _ = use(makeDiamondRef_VX_NoAttr())
  // let _ = use(makeDiamondRef_XV_Shared())
  // let _ = use(makeDiamondRef_VX_Shared())
}

InheritFRTTests.test("DiamondNoRef: diamond inheritance where the repeated base is not a ref") {
  let _ = use(makeDiamondNoRef_ARB())
  let _ = use(makeDiamondNoRef_RAB())
  let _ = use(makeDiamondNoRef_RARB())
}

runAllTests()
