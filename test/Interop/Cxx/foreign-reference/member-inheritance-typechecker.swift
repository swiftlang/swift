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
  a1.virtualMethod()
  a1.fooRename()  // expected-error {{value of type 'A1' has no member 'fooRename'}}
  a1.swiftFooRename()
  a1.swiftParamsRename(a1: 42)

  b1.virtualMethod()
  b1.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: ignoring swift_name 'swiftVirtualMethod()' in 'B1'; swift_name attributes have no effect on method overrides}}
  b1.fooRename()  // expected-error {{value of type 'B1' has no member 'fooRename'}}
  b1.swiftFooRename()
  b1.barRename()  // expected-error {{value of type 'B1' has no member 'barRename'}}
  b1.swiftBarRename()
  b1.B1BarRename()  // expected-error {{'B1BarRename()' is unavailable: ignoring swift_name 'B1BarRename()' in 'B1'; swift_name attributes have no effect on method overrides}}
  b1.swiftParamsRename(a1: 42)
  b1.swiftParamsRename(b1: 42)  // expected-error {{'swiftParamsRename(b1:)' is unavailable: ignoring swift_name 'swiftParamsRename(b1:)' in 'B1'; swift_name attributes have no effect on method overrides}}

  b2.virtualMethod()
  b2.fooRename()  // expected-error {{value of type 'B2' has no member 'fooRename'; did you mean 'swiftFooRename'?}}
  b2.swiftFooRename()
  b2.B2BarRename()  // expected-error {{'B2BarRename()' is unavailable: ignoring swift_name 'B2BarRename()' in 'B2'; swift_name attributes have no effect on method overrides}}
  b2.swiftParamsRename(a1: 42)

  c1.virtualMethod()
  c1.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: ignoring swift_name 'swiftVirtualMethod()' in 'B1'; swift_name attributes have no effect on method overrides}}
  c1.fooRename()  // expected-error {{value of type 'C1' has no member 'fooRename'}}
  c1.swiftFooRename()
  c1.barRename()  // expected-error {{value of type 'C1' has no member 'barRename'}}
  c1.swiftBarRename()
  c1.B1BarRename()  // expected-error {{'B1BarRename()' is unavailable: ignoring swift_name 'B1BarRename()' in 'B1'; swift_name attributes have no effect on method overrides}}
  c1.paramsRename(42)  // expected-error {{value of type 'C1' has no member 'paramsRename'}}
  c1.swiftParamsRename(42)  // expected-error {{missing argument label 'a1:' in call}}
  c1.swiftParamsRename(a1: 42)

  c2.virtualMethod()
  c2.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: ignoring swift_name 'swiftVirtualMethod()' in 'C2'; swift_name attributes have no effect on method overrides}}
  c2.fooRename()  // expected-error {{value of type 'C2' has no member 'fooRename'}}
  c2.swiftFooRename()
  c2.C2FooRename()  // expected-error {{'C2FooRename()' is unavailable: ignoring swift_name 'C2FooRename()' in 'C2'; swift_name attributes have no effect on method overrides}}
  c2.barRename()  // expected-error {{value of type 'C2' has no member 'barRename'}}
  c2.swiftBarRename()
  c2.B1BarRename()  // expected-error {{'B1BarRename()' is unavailable: ignoring swift_name 'B1BarRename()' in 'C2'; swift_name attributes have no effect on method overrides}}
  c2.paramsRename(42)  // expected-error {{value of type 'C2' has no member 'paramsRename'}}
  c2.swiftParamsRename(a1: 42)
  c2.swiftParamsRename(b1: 42)  // expected-error {{'swiftParamsRename(b1:)' is unavailable: ignoring swift_name 'swiftParamsRename(b1:)' in 'C2'; swift_name attributes have no effect on method overrides}}

  d1.virtualMethod()
  d1.swiftVirtualMethod()
  d1.fooRename()  // expected-error {{value of type 'D1' has no member 'fooRename'}}
  d1.swiftFooRename()  // expected-error {{ambiguous use of 'swiftFooRename()'}}
  d1.barRename()  // expected-error {{value of type 'D1' has no member 'barRename'}}
  d1.swiftBarRename()
  d1.A2BarRename()
  d1.swiftParamsRename(a1: 42)
  d1.swiftParamsRename(a2: 42)

  d2.virtualMethod()  // expected-error {{'virtualMethod()' is unavailable: overrides multiple C++ methods with different Swift names}}
  d2.swiftVirtualMethod()  // expected-error {{ambiguous use of 'swiftVirtualMethod()'}}
  d2.fooRename()  // expected-error {{value of type 'D2' has no member 'fooRename'}}
  d2.swiftFooRename()  // expected-error {{ambiguous use of 'swiftFooRename()'}}
  d2.barRename()  // expected-error {{value of type 'D2' has no member 'barRename'}}
  d2.swiftBarRename()  // expected-error {{'swiftBarRename()' is unavailable: overrides multiple C++ methods with different Swift names}}
  d2.A2BarRename()  // expected-error {{'A2BarRename()' is unavailable: overrides multiple C++ methods with different Swift names}}
  d2.swiftParamsRename(a1: 42)  // expected-error {{'swiftParamsRename(a1:)' is unavailable: overrides multiple C++ methods with different Swift names}}
  d2.swiftParamsRename(a2: 42)  // expected-error {{'swiftParamsRename(a2:)' is unavailable: overrides multiple C++ methods with different Swift names}}
  d2.swiftParamsRename(b1: 42)  // expected-error {{'swiftParamsRename(b1:)' is unavailable: ignoring swift_name 'swiftParamsRename(b1:)' in 'B1'; swift_name attributes have no effect on method overrides}}

  d3.virtualMethod()
  d3.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: ignoring swift_name 'swiftVirtualMethod()' in 'B1'; swift_name attributes have no effect on method overrides}}
  d3.fooRename()  // expected-error {{value of type 'D3' has no member 'fooRename'}}
  d3.swiftFooRename()
  d3.barRename()  // expected-error {{value of type 'D3' has no member 'barRename'}}
  d3.swiftBarRename()
  d3.B1BarRename()  // expected-error {{'B1BarRename()' is unavailable: ignoring swift_name 'B1BarRename()' in 'B1'; swift_name attributes have no effect on method overrides}}
  d3.swiftParamsRename(a1: 42)
  d3.swiftParamsRename(b1: 42)  // expected-error {{'swiftParamsRename(b1:)' is unavailable: ignoring swift_name 'swiftParamsRename(b1:)' in 'B1'; swift_name attributes have no effect on method overrides}}

  d4.virtualMethod()
  d4.swiftVirtualMethod()  // expected-error {{'swiftVirtualMethod()' is unavailable: ignoring swift_name 'swiftVirtualMethod()' in 'B1'; swift_name attributes have no effect on method overrides}}
  d4.fooRename()  // expected-error {{value of type 'D4' has no member 'fooRename'}}
  d4.swiftFooRename()  // expected-error {{ambiguous use of 'swiftFooRename()'}}
  d4.barRename()  // expected-error {{value of type 'D4' has no member 'barRename'}}
  d4.swiftBarRename()  // expected-error {{ambiguous use of 'swiftBarRename()'}}
  d4.B1BarRename()  // expected-error {{'B1BarRename()' is unavailable: ignoring swift_name 'B1BarRename()' in 'B1'; swift_name attributes have no effect on method overrides}}
  d4.paramsRename(42)  // expected-error {{value of type 'D4' has no member 'paramsRename'}}
  d4.swiftParamsRename(42)  // expected-error {{missing argument label 'a1:' in call}}
  d4.swiftParamsRename(a1: 42)
}
