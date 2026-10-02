// RUN: %target-swift-frontend -typecheck -verify -I %S%{fs-sep}Inputs %s -cxx-interoperability-mode=default -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}constructors.h -Xcc -Wno-nullability-completeness

// XFAIL: OS=linux-androideabi

import Constructors

let _ = SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PlacementOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedOperatorNew' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedOperatorNew' cannot be constructed because it has no accessible initializers}}

let _ = SwiftInitSynthesisForCXXRefTypes.PrivateCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.PrivateCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.ProtectedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.ProtectedCtor' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.DeletedCtor()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.DeletedCtor' cannot be constructed because it has no accessible initializers}}

@available(SwiftStdlib 5.8, *)
func ctorsWithDefaultArgs() {
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg()  // expected-warning {{cannot infer ownership of foreign reference value returned by 'init(_:)'}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg(1)  // expected-warning {{cannot infer ownership of foreign reference value returned by 'init(_:)'}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultArg(1, 2)  // expected-error {{extra argument in call}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg()  // expected-error {{missing argument for parameter #1 in call}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1)  // expected-warning {{cannot infer ownership of foreign reference value returned by 'init(_:_:)'}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1, 2)  // expected-warning {{cannot infer ownership of foreign reference value returned by 'init(_:_:)'}}
  let _ = SwiftInitSynthesisForCXXRefTypes.CtorWithDefaultAndNonDefaultArg(1, 2, 3)  // expected-error {{extra argument in call}}
}

let _ = SwiftInitSynthesisForCXXRefTypes.VariadicCtors()  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.VariadicCtors' cannot be constructed because it has no accessible initializers}}
let _ = SwiftInitSynthesisForCXXRefTypes.VariadicCtors(1)  // expected-error {{'SwiftInitSynthesisForCXXRefTypes.VariadicCtors' cannot be constructed because it has no accessible initializers}}
