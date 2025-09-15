// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-5.9
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-6

import MemberInheritance

@available(SwiftStdlib 5.8, *)
func takesImmortalBase(_ i: ImmortalBase) {
  let _ = i.get42()
  let _ = i.getOverridden42()
  let _ = i.getIntValue()
}

@available(SwiftStdlib 5.8, *)
func takesImmortal(_ i: Immortal) {
  let _ = i.get42()
  let _ = i.getOverridden42()
  let _ = i.getIntValue()
  i.setIntValue(1)
}

@available(SwiftStdlib 5.8, *)
func takesDerivedFromImmortal(_ i: DerivedFromImmortal) {
  let _ = i.get42()
  let _ = i.getOverridden42()
  let _ = i.getIntValue()
  i.setIntValue(1)
}

@available(SwiftStdlib 5.8, *)
func callsRenamedVirtualMethodsInFRT(_ i: Immortal2) {
  i.virtualRename()  // expected-error {{value of type 'Immortal2' has no member 'virtualRename'}}
  i.swiftVirtualRename()
}

@available(SwiftStdlib 5.8, *)
func callsOverridesOfRenamedVirtualMethods(
  _ a1: A1, _ a2: A2, _ b1: B1, _ b2: B2, _ c1: C1, _ c2: C2, _ d1: D1, _ d2: D2, _ d3: D3, _ d4: D4
) {
  let _ = a1.virtualMethod()
  let _ = a1.fooRename()  // expected-error {{value of type 'A1' has no member 'fooRename'}}
  let _ = a1.swiftFooRename()
  let _ = a1.swiftParamsRename(a1: 42)

  let _ = b1.virtualMethod()
  let _ = b1.swiftVirtualMethod()  // expected-error {{value of type 'B1' has no member 'swiftVirtualMethod'}}
  let _ = b1.fooRename()  // expected-error {{value of type 'B1' has no member 'fooRename'}}
  let _ = b1.swiftFooRename()
  let _ = b1.barRename()  // expected-error {{value of type 'B1' has no member 'barRename'}}
  let _ = b1.swiftBarRename()
  let _ = b1.B1BarRename()  // expected-error {{value of type 'B1' has no member 'B1BarRename'; did you mean 'swiftBarRename'}}
  let _ = b1.swiftParamsRename(a1: 42)
  let _ = b1.swiftParamsRename(b1: 42)  // expected-error {{incorrect argument label in call (have 'b1:', expected 'a1:')}}

  let _ = b2.virtualMethod()
  let _ = b2.fooRename()  // expected-error {{value of type 'B2' has no member 'fooRename'; did you mean 'swiftFooRename'?}}
  let _ = b2.swiftFooRename()
  let _ = b2.B2BarRename()  // expected-error {{value of type 'B2' has no member 'B2BarRename'; did you mean 'swiftBarRename'}}
  let _ = b2.swiftParamsRename(a1: 42)

  let _ = c1.virtualMethod()
  let _ = c1.swiftVirtualMethod()  // expected-error {{value of type 'C1' has no member 'swiftVirtualMethod'}}
  let _ = c1.fooRename()  // expected-error {{value of type 'C1' has no member 'fooRename'}}
  let _ = c1.swiftFooRename()
  let _ = c1.barRename()  // expected-error {{value of type 'C1' has no member 'barRename'}}
  let _ = c1.swiftBarRename()
  let _ = c1.B1BarRename()  // expected-error {{value of type 'C1' has no member 'B1BarRename'}}
  let _ = c1.paramsRename(42)  // expected-error {{value of type 'C1' has no member 'paramsRename'}}
  let _ = c1.swiftParamsRename(42)  // expected-error {{missing argument label 'a1:' in call}}
  let _ = c1.swiftParamsRename(a1: 42)

  let _ = c2.virtualMethod()
  let _ = c2.swiftVirtualMethod()  // expected-error {{value of type 'C2' has no member 'swiftVirtualMethod'}}
  let _ = c2.fooRename()  // expected-error {{value of type 'C2' has no member 'fooRename'}}
  let _ = c2.swiftFooRename()
  let _ = c2.C2FooRename()  // expected-error {{value of type 'C2' has no member 'C2FooRename'}}
  let _ = c2.barRename()  // expected-error {{value of type 'C2' has no member 'barRename'}}
  let _ = c2.swiftBarRename()
  let _ = c2.B1BarRename()  // expected-error {{value of type 'C2' has no member 'B1BarRename'}}
  let _ = c2.paramsRename(42)  // expected-error {{value of type 'C2' has no member 'paramsRename'}}
  let _ = c2.swiftParamsRename(a1: 42)
  let _ = c2.swiftParamsRename(b1: 42)  // expected-error {{incorrect argument label in call (have 'b1:', expected 'a1:')}}

  let _ = d1.virtualMethod()
  let _ = d1.swiftVirtualMethod()
  let _ = d1.fooRename()  // expected-error {{value of type 'D1' has no member 'fooRename'}}
  let _ = d1.swiftFooRename()  // expected-error {{ambiguous use of 'swiftFooRename()'}}
  let _ = d1.barRename()  // expected-error {{value of type 'D1' has no member 'barRename'}}
  let _ = d1.swiftBarRename()
  let _ = d1.A2BarRename()
  let _ = d1.swiftParamsRename(a1: 42)
  let _ = d1.swiftParamsRename(a2: 42)

  let _ = d2.virtualMethod()  // expected-error {{'virtualMethod()' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.fooRename()  // expected-error {{value of type 'D2' has no member 'fooRename'}}
  let _ = d2.swiftFooRename()
  let _ = d2.barRename()  // expected-error {{value of type 'D2' has no member 'barRename'}}
  let _ = d2.swiftBarRename()  // expected-error {{'swiftBarRename()' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.A2BarRename()  // expected-error {{'A2BarRename()' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.swiftParamsRename(a1: 42)  // expected-error {{'swiftParamsRename(a1:)' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.swiftParamsRename(a2: 42)  // expected-error {{'swiftParamsRename(a2:)' is unavailable: overrides multiple C++ methods with different Swift names}}
  let _ = d2.swiftParamsRename(b1: 42)  // expected-error {{no exact matches in call to instance method 'swiftParamsRename'}}

  let _ = d3.virtualMethod() // expected-error {{ambiguous use of 'virtualMethod()'}}
  let _ = d3.swiftVirtualMethod()  // expected-error {{value of type 'D3' has no member 'swiftVirtualMethod'}}
  let _ = d3.fooRename()  // expected-error {{value of type 'D3' has no member 'fooRename'}}
  let _ = d3.swiftFooRename() // expected-error {{ambiguous use of 'swiftFooRename()'}}
  let _ = d3.barRename()  // expected-error {{value of type 'D3' has no member 'barRename'}}
  let _ = d3.swiftBarRename() // expected-error {{ambiguous use of 'swiftBarRename()'}}
  let _ = d3.B1BarRename()  // expected-error {{value of type 'D3' has no member 'B1BarRename'}}
  let _ = d3.swiftParamsRename(a1: 42) // expected-error {{ambiguous use of 'swiftParamsRename(a1:)'}}
  let _ = d3.swiftParamsRename(b1: 42)  // expected-error {{no exact matches in call to instance method 'swiftParamsRename'}}

  let _ = d4.virtualMethod() // expected-error {{ambiguous use of 'virtualMethod()'}}
  let _ = d4.swiftVirtualMethod()  // expected-error {{value of type 'D4' has no member 'swiftVirtualMethod'}}
  let _ = d4.fooRename()  // expected-error {{value of type 'D4' has no member 'fooRename'}}
  let _ = d4.swiftFooRename()  // expected-error {{ambiguous use of 'swiftFooRename()'}}
  let _ = d4.barRename()  // expected-error {{value of type 'D4' has no member 'barRename'}}
  let _ = d4.swiftBarRename()  // expected-error {{ambiguous use of 'swiftBarRename()'}}
  let _ = d4.B1BarRename()  // expected-error {{value of type 'D4' has no member 'B1BarRename'}}
  let _ = d4.paramsRename(42)  // expected-error {{value of type 'D4' has no member 'paramsRename'}}
  let _ = d4.swiftParamsRename(42) // expected-error {{no exact matches in call to instance method 'swiftParamsRename'}}
  let _ = d4.swiftParamsRename(a1: 42) // expected-error {{ambiguous use of 'swiftParamsRename(a1:)'}}
}

@available(SwiftStdlib 5.8, *)
func callsOverridesOfValueTypeMethods (_ frt: DerivedFRTValueType, _ empty_frt: EmptyDerivedFRTValueType, _ vt: DerivedValueType) {
  let _ = frt.virtualMethod()
  let _ = frt.renameMethodBase() // expected-error {{value of type 'DerivedFRTValueType' has no member 'renameMethodBase'}}
  let _ = frt.swiftRenameMethodBase()
  let _ = frt.renameMethodDerived()
  let _ = frt.swiftRenameMethodDerived() // expected-error {{value of type 'DerivedFRTValueType' has no member 'swiftRenameMethodDerived'}}

  let _ = frt.pureVirtualMethod()
  let _ = frt.pureRenameBase() // expected-error {{value of type 'DerivedFRTValueType' has no member 'pureRenameBase'}}
  let _ = frt.swiftPureRenameBase()
  let _ = frt.pureRenameDerived()
  let _ = frt.swiftPureRenameDerived() // expected-error {{value of type 'DerivedFRTValueType' has no member 'swiftPureRenameDerived'}}

  let _ = empty_frt.virtualMethod()
  let _ = empty_frt.renameMethodBase() // expected-error {{value of type 'EmptyDerivedFRTValueType' has no member 'renameMethodBase'}}
  let _ = empty_frt.swiftRenameMethodBase()
  let _ = empty_frt.renameMethodDerived()

  let _ = empty_frt.pureVirtualMethod() // expected-error {{'pureVirtualMethod()' is unavailable: virtual function is not available in Swift because it is pure}}

  let _ = vt.virtualMethod()
  let _ = vt.renameMethodBase() // expected-error {{value of type 'DerivedValueType' has no member 'renameMethodBase'}}
  let _ = vt.swiftRenameMethodBase()
  let _ = vt.renameMethodDerived()
  let _ = vt.swiftRenameMethodDerived() // expected-error {{value of type 'DerivedValueType' has no member 'swiftRenameMethodDerived'}}

  let _ = vt.pureVirtualMethod()
  let _ = vt.pureRenameBase() // expected-error {{value of type 'DerivedValueType' has no member 'pureRenameBase'}}
  let _ = vt.swiftPureRenameBase()
  let _ = vt.pureRenameDerived()
  let _ = vt.swiftPureRenameDerived() // expected-error {{value of type 'DerivedValueType' has no member 'swiftPureRenameDerived'}}
}

@available(SwiftStdlib 5.8, *)
func callsOverridesOfAbstractFRTMethods (_ frt: DerivedAbstractFRT, _ empty_frt: EmptyDerivedAbstractFRT) {
  let _ = frt.pureVirtualMethod()
  let _ = frt.pureRenameBase() // expected-error {{value of type 'DerivedAbstractFRT' has no member 'pureRenameBase'}}
  let _ = frt.swiftPureRenameBase()
  let _ = frt.pureRenameDerived()
  let _ = frt.swiftPureRenameDerived() // expected-error {{value of type 'DerivedAbstractFRT' has no member 'swiftPureRenameDerived'}}

  let _ = empty_frt.pureVirtualMethod()
  let _ = empty_frt.pureRenameBase() // expected-error {{value of type 'EmptyDerivedAbstractFRT' has no member 'pureRenameBase'}}
  let _ = empty_frt.swiftPureRenameBase()
  let _ = empty_frt.pureRenameDerived()
  let _ = empty_frt.swiftPureRenameDerived() // expected-error {{value of type 'EmptyDerivedAbstractFRT' has no member 'swiftPureRenameDerived'}}
}

