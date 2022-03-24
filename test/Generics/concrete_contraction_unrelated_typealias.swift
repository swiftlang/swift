// R/UN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-inferred-signatures=on 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-inferred-signatures=on -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

// Another GenericSignatureBuilder oddity, reduced from RxSwift.
//
// The requirements 'Proxy.Parent == P' and 'Proxy.Delegate == D' in the
// init() below refer to both the typealias and the associated type,
// despite the class being unrelated to the protocol; it just happens to
// define typealiases with the same name.
//
// In the Requirement Machine, the concrete contraction pre-processing
// pass would eagerly substitute the concrete type into these two
// requirements, producing the useless requirements 'P == P' and 'D == D'.
//
// Make sure concrete contraction keeps these requirements as-is by
// checking the generic signature with and without concrete contraction.

class GenericDelegateProxy<P : AnyObject, D> {
  typealias Parent = P
  typealias Delegate = D

  // CHECK-LABEL: .GenericDelegateProxy.init(_:)@
  // CHECK-NEXT: <P, D, Proxy where P == Proxy.[DelegateProxyType]Parent, D == Proxy.[DelegateProxyType]Delegate, Proxy : GenericDelegateProxy<P, D>, Proxy : DelegateProxyType>
  init<Proxy: DelegateProxyType>(_: Proxy.Type)
    where Proxy: GenericDelegateProxy<P, D>,
          Proxy.Parent == P,
          Proxy.Delegate == D {}
}

class SomeClass {}
struct SomeStruct {}

class ConcreteDelegateProxy {
  typealias Parent = SomeClass
  typealias Delegate = SomeStruct

  // CHECK-LABEL: .ConcreteDelegateProxy.init(_:_:_:)@
  // CHECK-NEXT: <P, D, Proxy where P == Proxy.[DelegateProxyType]Parent, D == Proxy.[DelegateProxyType]Delegate, Proxy : ConcreteDelegateProxy, Proxy : DelegateProxyType>
  init<P, D, Proxy: DelegateProxyType>(_: P, _: D, _: Proxy.Type)
    where Proxy: ConcreteDelegateProxy,
          Proxy.Parent == P,
          Proxy.Delegate == D {}
}

protocol DelegateProxyType {
  associatedtype Parent : AnyObject
  associatedtype Delegate
}