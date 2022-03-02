// RUN: %target-swift-frontend -emit-silgen %s -verify -warn-implicit-overrides | %FileCheck %s

protocol Base {
  func foo1<T : P>(_: T)
  func foo2<T : P>(_: T, _: T.T)
  func foo3<T : P>(_: T, _: T.T)
}

protocol Derived : Base {
  func foo1<T : P>(_: T)
  func foo2<T : P>(_: T, _: T.T)
  func foo3<T : Q>(_: T, _: T.T)
}

protocol P {
  associatedtype T
}

protocol Q {
  associatedtype T
}

struct S : Derived {
  func foo1<T : P>(_: T) {}
  func foo2<T : P>(_: T, _: T.T) {}
  func foo3<T : P>(_: T, _: T.T) {}
  func foo3<T : Q>(_: T, _: T.T) {}
}

// Make sure that Derived.foo1 and Derived.foo2 are not counted as overrides of
// Base.foo1 and Base.foo2 respectively. Even though their types match, bugs
// in Swift 5.6 and earlier prevented them from being overrides. We can't fix
// it now because it would be an ABI break.

// CHECK-LABEL: sil_witness_table hidden S: Derived module override_generic {
// CHECK-NEXT: base_protocol Base: S: Base module override_generic
// CHECK-NEXT: method #Derived.foo1: <Self where Self : Derived><T where T : P> (Self) -> (T) -> () : @$s16override_generic1SVAA7DerivedA2aDP4foo1yyqd__AA1PRd__lFTW
// CHECK-NEXT: method #Derived.foo2: <Self where Self : Derived><T where T : P> (Self) -> (T, T.T) -> () : @$s16override_generic1SVAA7DerivedA2aDP4foo2yyqd___1TQyd__tAA1PRd__lFTW
// CHECK-NEXT: method #Derived.foo3: <Self where Self : Derived><T where T : Q> (Self) -> (T, T.T) -> () : @$s16override_generic1SVAA7DerivedA2aDP4foo3yyqd___1TQyd__tAA1QRd__lFTW
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden S: Base module override_generic {
// CHECK-NEXT: method #Base.foo1: <Self where Self : Base><T where T : P> (Self) -> (T) -> () : @$s16override_generic1SVAA4BaseA2aDP4foo1yyqd__AA1PRd__lFTW
// CHECK-NEXT: method #Base.foo2: <Self where Self : Base><T where T : P> (Self) -> (T, T.T) -> () : @$s16override_generic1SVAA4BaseA2aDP4foo2yyqd___1TQyd__tAA1PRd__lFTW
// CHECK-NEXT: method #Base.foo3: <Self where Self : Base><T where T : P> (Self) -> (T, T.T) -> () : @$s16override_generic1SVAA4BaseA2aDP4foo3yyqd___1TQyd__tAA1PRd__lFTW
// CHECK-NEXT: }
