// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-5.9
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=swift-6
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import UsingBaseMembers

let a = PublicBasePrivateInheritance()
let _ = a.publicGetter()
a.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let b = PublicBaseProtectedInheritance()
let _ = b.publicGetter()
b.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let _ = UsingBaseConstructorWithParam(566 as Int32)
let _ = UsingBaseConstructorWithParam(566 as UInt32)

let _ = UsingBaseConstructorEmpty()
