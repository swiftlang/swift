// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -typo-correction-limit 0
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-5.9 -typo-correction-limit 0
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-6 -typo-correction-limit 0

import VirtualMethods

let _ = Base() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}
let _ = Base2() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}

let _ = DerivedInt()
let _ = Derived2()
let _ = Derived3()
let _ = Derived4()
let _ = DerivedFromDerived2()

VirtualNonAbstractBase().nonAbstractMethod()

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

func callVirtualRenamedMethod(_ b: Base) {
  b.virtualRename()  // expected-error {{value of type 'Base' has no member 'virtualRename'}}
  b.swiftVirtualRename()
}

@available(SwiftStdlib 5.8, *)
func callsRenamedVirtualMethodsInFRT(_ i: Immortal2) {
  i.virtualRename()  // expected-error {{value of type 'Immortal2' has no member 'virtualRename'}}
  i.swiftVirtualRename()
}
