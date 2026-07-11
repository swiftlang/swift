// An opaque-return ('some P') function imported from another module crashed the SIL verifier
// under Embedded: the client deserialized the body (whose indirect result is the concrete
// underlying type) but could not substitute the opaque result type back to a non-public
// underlying type, so the body and the function's @_opaqueReturnTypeOf SIL type diverged.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t -parse-as-library %t/MyModule.swift -o %t/MyModule.o -emit-module -emit-module-path %t/MyModule.swiftmodule -emit-empty-object-file

// The opaque result must lower to its concrete underlying type across the module boundary:
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature Embedded | %FileCheck %t/Main.swift --check-prefix=CHECK-IR

// It must also build and run end to end:
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t %t/Main.swift -o %t/Main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %t/Main.swift

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- MyModule.swift

public protocol P { func f() }

struct Impl: P { func f() { print("Impl.f") } }
public func makeOpaque() -> some P { Impl() }

// Positive control: public underlying type, already substituted cross-module before the fix.
public struct PubImpl: P { public func f() { print("Pub.f") } }
public func makePublic() -> some P { PubImpl() }

// Generic internal underlying type.
struct GenImpl<T>: P { func f() { print("Gen.f") } }
public func makeGeneric<T>(_: T.Type) -> some P { GenImpl<T>() }

// fileprivate underlying type (less accessible than internal, so it also crashed before the fix).
fileprivate struct FImpl: P { func f() { print("FP.f") } }
public func makeFilePrivate() -> some P { FImpl() }

// Opaque-of-opaque: the underlying type is itself an opaque type.
public func makeOuter() -> some P { makeOpaque() }

//--- Main.swift

import MyModule

// Type-substitution path (canSubstituteTypeInto via the type operator()).
@inline(never) public func triggerType() { _ = makeOpaque() }

// Conformance-substitution path (canSubstituteTypeInto via the conformance operator()).
@inline(never) public func triggerConformance() { makeOpaque().f() }

// Each opaque result must lower to its concrete underlying type cross-module.
@inline(never) public func triggerEdgeCases() {
  makePublic().f()
  makeGeneric(Int.self).f()
  makeFilePrivate().f()
  makeOuter().f()
}

makeOpaque().f()

// makeOpaque()'s opaque result lowers to the concrete underlying type at the call site.
// CHECK-IR-LABEL: define {{.*}}@"$e4Main18triggerConformanceyyF"
// CHECK-IR:       call swiftcc void @"$e8MyModule10makeOpaqueQryF"(ptr noalias sret(%T8MyModule4ImplV)

// CHECK: Impl.f
