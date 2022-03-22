// RUN: %target-swift-frontend -typecheck -verify %s -disable-availability-checking -debug-generic-signatures -requirement-machine-inferred-signatures=on -enable-requirement-machine-opaque-archetypes 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T : P2
  associatedtype U
}

struct S_P1 : P1 {
  typealias T = S_P2
  typealias U = Int
}

protocol P2 {}

struct S_P2 : P2 {}

protocol P {
  associatedtype T

  var t: T { get }
}

struct DefinesOpaqueP1 : P {
  var t: some P1 {
    return S_P1()
  }
}

struct ConcreteHasP<T : P1, TT : P2, TU> {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=ConcreteHasP
// CHECK-NEXT: Generic signature: <T, TT, TU where T == some P1, TT == (some P1).T, TU == (some P1).U>
extension ConcreteHasP where T == DefinesOpaqueP1.T, TT == T.T, TU == T.U {
  func checkSameType1(_ t: TT) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType2(_ u: TU) -> DefinesOpaqueP1.T.U { return u }

  func checkSameType3(_ t: T.T) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType4(_ u: T.U) -> DefinesOpaqueP1.T.U { return u }
}

struct G<T> {}

protocol HasP {
  associatedtype T : P1
  associatedtype U
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=HasP
// CHECK-NEXT: Generic signature: <Self where Self : HasP, Self.[HasP]T == some P1, Self.[HasP]U == G<(some P1).T>>
extension HasP where T == DefinesOpaqueP1.T, U == G<T.T> {
  func checkSameType1(_ t: T.T) -> DefinesOpaqueP1.T.T { return t }
  func checkSameType2(_ u: T.U) -> DefinesOpaqueP1.T.U { return u }
}

// FIXME: This does not work with -enable-requirement-machine-opaque-archetypes.
// See opaque_archetype_concrete_requirement_recursive.swift for a demonstration
// that it works without the flag (but more involved examples like the above
// won't work).

protocol RecursiveP {
  associatedtype T : RecursiveP
}

struct S_RecursiveP : RecursiveP {
  typealias T = S_RecursiveP
}

struct DefinesRecursiveP : P {
  var t: some RecursiveP {
    return S_RecursiveP()
  }
}

protocol HasRecursiveP {
  associatedtype T : RecursiveP
}

extension HasRecursiveP where T == DefinesRecursiveP.T {}
// expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[HasRecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[concrete: (((((((((@_opaqueReturnTypeOf("$s37opaque_archetype_concrete_requirement17DefinesRecursivePV1tQrvp", 0) __.T).T).T).T).T).T).T).T).T).T] => τ_0_0.[HasRecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T]}}

