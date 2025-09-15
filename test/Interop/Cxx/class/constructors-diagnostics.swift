// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=default -disable-availability-checking -verify-additional-file %S/Inputs/constructors.h -Xcc -Wno-nullability-completeness

// This test uses -verify-additional-file, which do not work well on Windows.
// UNSUPPORTED: OS=windows-msvc
// XFAIL: OS=linux-androideabi

import Constructors

let _ = SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedCtor' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg(1)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg(1, 2)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1, 2)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1, 2, 3)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.VariadicCtors()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.VariadicCtors' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.VariadicCtors(1)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.VariadicCtors' cannot be constructed because it has no accessible initializers}}
