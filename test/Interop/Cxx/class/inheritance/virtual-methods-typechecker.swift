// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import VirtualMethods

let _ = Base() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}
let _ = Base2() // expected-error {{'init()' is unavailable: constructors of abstract C++ classes are unavailable in Swift}}

let _ = DerivedInt()
let _ = Derived2()
let _ = Derived3()
let _ = Derived4()
let _ = DerivedFromDerived2()

VirtualNonAbstractBase().nonAbstractMethod()
