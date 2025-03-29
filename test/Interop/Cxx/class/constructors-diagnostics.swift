// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=default -enable-experimental-feature CXXForeignReferenceTypeInitializers -disable-availability-checking -verify-additional-file %S/Inputs/constructors.h -Xcc -Wno-nullability-completeness

// This test uses -verify-additional-file, which do not work well on Windows:
// UNSUPPORTED: OS=windows-msvc
// REQUIRES: swift_feature_CXXForeignReferenceTypeInitializers

import Constructors

let _ = PlacementOperatorNew.CxxRefTy()  // expected-error {{'PlacementOperatorNew.CxxRefTy' cannot be constructed because it has no accessible initializers}}

let _ = PrivateOperatorNew.CxxRefTy()  // expected-error {{'PrivateOperatorNew.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = ProtectedOperatorNew.CxxRefTy()  // expected-error {{'ProtectedOperatorNew.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = DeletedOperatorNew.CxxRefTy()  // expected-error {{'DeletedOperatorNew.CxxRefTy' cannot be constructed because it has no accessible initializers}}

let _ = PrivateCtor.CxxRefTy()  // expected-error {{'PrivateCtor.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = ProtectedCtor.CxxRefTy()  // expected-error {{'ProtectedCtor.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = DeletedCtor.CxxRefTy()  // expected-error {{'DeletedCtor.CxxRefTy' cannot be constructed because it has no accessible initializers}}

let _ = CtorWithDefaultArg.CxxRefTy()  // expected-error {{'CtorWithDefaultArg.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = CtorWithDefaultAndNonDefaultArg.CxxRefTy()  // expected-error {{'CtorWithDefaultAndNonDefaultArg.CxxRefTy' cannot be constructed because it has no accessible initializers}}

let _ = DefaulltAndNonDefaultCtors.CxxRefTy()
// TODO: change the error message when we start supporting parameterised ctors
let _ = DefaulltAndNonDefaultCtors.CxxRefTy(2)  // expected-error {{argument passed to call that takes no arguments}}

let _ = ParameterisedCtor.CxxRefTy()  // expected-error {{'ParameterisedCtor.CxxRefTy' cannot be constructed because it has no accessible initializers}}
let _ = ParameterisedCtor.CxxRefTy(3)  // expected-error {{'ParameterisedCtor.CxxRefTy' cannot be constructed because it has no accessible initializers}}

let _ = ImmortalReference.CxxRefTy()
