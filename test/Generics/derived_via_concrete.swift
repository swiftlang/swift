// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -debug-generic-signatures -typecheck %s 2>&1 | %FileCheck %s

protocol P {}
class C {}

class X<T : P> : U {}
class Y<T : C> : V {}
class Z<T : AnyObject> : W {}

protocol U {
  associatedtype T : P
}

protocol V {
  associatedtype T : C
}

protocol W {
  associatedtype T : AnyObject
}

// CHECK: Generic signature: <A, B where A : X<B>, B : P>
func derivedViaConcreteX1<A, B>(_: A, _: B)
  where A : U, A : X<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'U'}}
// expected-note@-2 {{conformance constraint 'A' : 'U' implied here}}

// CHECK: Generic signature: <A, B where A : X<B>, B : P>
func derivedViaConcreteX2<A, B>(_: A, _: B)
  where A : U, B : P, A : X<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'U'}}
// expected-note@-2 {{conformance constraint 'A' : 'U' implied here}}

// CHECK: Generic signature: <A, B where A : Y<B>, B : C>
func derivedViaConcreteY1<A, B>(_: A, _: B)
  where A : V, A : Y<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'V'}}
// expected-note@-2 {{conformance constraint 'A' : 'V' implied here}}

// CHECK: Generic signature: <A, B where A : Y<B>, B : C>
func derivedViaConcreteY2<A, B>(_: A, _: B)
  where A : V, B : C, A : Y<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'V'}}
// expected-note@-2 {{conformance constraint 'A' : 'V' implied here}}

// CHECK: Generic signature: <A, B where A : Z<B>, B : AnyObject>
func derivedViaConcreteZ1<A, B>(_: A, _: B)
  where A : W, A : Z<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'W'}}
// expected-note@-2 {{conformance constraint 'A' : 'W' implied here}}

// CHECK: Generic signature: <A, B where A : Z<B>, B : AnyObject>
func derivedViaConcreteZ2<A, B>(_: A, _: B)
  where A : W, B : AnyObject, A : Z<B> {}
// expected-warning@-1 {{redundant conformance constraint 'A' : 'W'}}
// expected-note@-2 {{conformance constraint 'A' : 'W' implied here}}
