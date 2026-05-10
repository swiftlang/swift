// RUN: %target-typecheck-verify-swift -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default

import VirtualMethods

let _ = Base() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}
let _ = DerivedInt()

let _ = Base2() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}

let _ = Derived2()
let _ = Derived3()
let _ = Derived4()
let _ = DerivedFromDerived2()

let vrb = VirtualRenamedBase()
let _ = vrb.cxxName() // expected-error {{has no member 'cxxName'}}
let _ = vrb.swiftName()
let vri = VirtualRenamedInherited()
let _ = vri.cxxName() // expected-error {{has no member 'cxxName'}}
let _ = vri.swiftName()
let vro = VirtualRenamedOverridden()
let _ = vro.cxxName() // expected-error {{has no member 'cxxName'}}
let _ = vro.swiftName()

func check(pvrb: PureVirtualRenamedBase) {
  let _ = pvrb.cxxName() // expected-error {{has no member 'cxxName'}}
  let _ = pvrb.swiftName() // expected-error {{virtual function is not available in Swift because it is pure}}
}
func check(pvri: PureVirtualRenamedInherited) {
  let _ = pvri.cxxName() // expected-error {{has no member 'cxxName'}}
  let _ = pvri.swiftName() // expected-error {{virtual function is not available in Swift because it is pure}}
}
let pvro = PureVirtualRenamedOverridden()
let _ = pvro.cxxName() // expected-error {{has no member 'cxxName'}}
let _ = pvro.swiftName()

VirtualNonAbstractBase().nonAbstractMethod()
