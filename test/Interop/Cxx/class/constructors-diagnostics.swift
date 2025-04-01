// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=default -enable-experimental-feature CXXForeignReferenceTypeInitializers -disable-availability-checking -verify-additional-file %S/Inputs/constructors.h -Xcc -Wno-nullability-completeness

// This test uses -verify-additional-file, which do not work well on Windows:
// UNSUPPORTED: OS=windows-msvc
// REQUIRES: swift_feature_CXXForeignReferenceTypeInitializers

import Constructors

let _ = SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedCtor' cannot be constructed because it has no accessible initializers}}

// Todo: add support for default args in ctors and fix this error
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg()  // expected-error {{missing argument for parameter #1 in call}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg()  // expected-error {{missing arguments for parameters #1, #2 in call}}
