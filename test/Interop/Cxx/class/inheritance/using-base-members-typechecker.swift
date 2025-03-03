// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import UsingBaseMembers

let a = PublicBasePrivateInheritance()
let _ = a.publicGetter()
a.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let b = PublicBaseProtectedInheritance()
let _ = b.publicGetter()
b.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let c = PublicBaseUsingPrivateTypedef()
let _ = c.publicGetter()
c.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let d = PublicBaseUsingPrivateUsingType()
let _ = d.publicGetter()
d.notExposed() // expected-error {{'notExposed' is inaccessible due to 'private' protection level}}

let _ = UsingBaseConstructorWithParam(566 as Int32)
let _ = UsingBaseConstructorWithParam(566 as UInt32)

let _ = UsingBaseConstructorEmpty()

let p = ProtectedMemberPrivateInheritance()
let _ = p.protectedGetter()

let o = OperatorBasePrivateInheritance()
if Bool(fromCxx: o) {
  let _: Int32 = o.pointee
} else if Bool(fromCxx: !o) {
  // let _: Int32 = o[789] // FIXME: currently broken
}
