
// RUN: %target-swift-emit-silgen -module-name witness_same_type %s | %FileCheck %s
// RUN: %target-swift-emit-ir -module-name witness_same_type %s

protocol Fooable {
  associatedtype Bar

  func foo<T: Fooable>(x x: T) -> Self.Bar where T.Bar == Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17witness_same_type3FooVAA7FooableA2aDP3foo1x3BarQzqd___tAaDRd__AHQyd__AIRSlFTW : $@convention(witness_method: Fooable) <τ_0_0 where τ_0_0 : Fooable, τ_0_0.Bar == X> (@in_guaranteed τ_0_0, @in_guaranteed Foo) -> @out X
struct Foo: Fooable {
  typealias Bar = X

  func foo<T: Fooable>(x x: T) -> X where T.Bar == X { return X() }
}

// rdar://problem/19049566
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s17witness_same_type14LazySequenceOfVyxq_GSTAAST12makeIterator0H0QzyFTW : $@convention(witness_method: Sequence) <τ_0_0, τ_0_1 where τ_0_0 : Sequence, τ_0_1 == τ_0_0.Element> (@in LazySequenceOf<τ_0_0, τ_0_1>) -> @out AnyIterator<τ_0_1>
public struct LazySequenceOf<SS : Sequence, A> : Sequence where SS.Iterator.Element == A {
  public func makeIterator() -> AnyIterator<A> { 
    var opt: AnyIterator<A>?
    return opt!
  }
	public subscript(i : Int) -> A { 
    get { 
      var opt: A?
      return opt!
    } 
  }
}

// rdar://problem/155465011

// Due to a series of unfortunate coincidences, we would assign the same mangling
// to the witness thunks for P1.f() in the S1: P1 and S2: P1 conformance.

public protocol P1 {
  func f<T: P2>(_: T) where T.A == Self
}

public protocol P2 {
  associatedtype A: P1
}

struct N1: P2 {
  typealias A = S1
}

struct N2: P2 {
  typealias A = S2
}

struct S1: P1 {
  func f<T: P2>(_: T) where T.A == Self {
    print("Hello from S1")
  }
}

struct S2: P1 {
  func f<T: P2>(_: T) where T.A == Self {
    print("Hello from S2")
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17witness_same_type2S1VAA2P1A2aDP1fyyqd__1AQyd__RszAA2P2Rd__lFTW : $@convention(witness_method: P1) <τ_0_0 where τ_0_0 : P2, τ_0_0.A == S1> (@in_guaranteed τ_0_0, @in_guaranteed S1) -> () {

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s17witness_same_type2S2VAA2P1A2aDP1fyyqd__1AQyd__RszAA2P2Rd__lFTW : $@convention(witness_method: P1) <τ_0_0 where τ_0_0 : P2, τ_0_0.A == S2> (@in_guaranteed τ_0_0, @in_guaranteed S2) -> () {

// CHECK-LABEL: sil_witness_table hidden N1: P2 module witness_same_type {
// CHECK-NEXT:    associated_conformance (A: P1): S1: P1 module witness_same_type
// CHECK-NEXT:    associated_type A: S1
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table hidden N2: P2 module witness_same_type {
// CHECK-NEXT:    associated_conformance (A: P1): S2: P1 module witness_same_type
// CHECK-NEXT:    associated_type A: S2
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table hidden S1: P1 module witness_same_type {
// CHECK-NEXT:    method #P1.f: <Self><T where Self == T.A, T : P2> (Self) -> (T) -> () : @$s17witness_same_type2S1VAA2P1A2aDP1fyyqd__1AQyd__RszAA2P2Rd__lFTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table hidden S2: P1 module witness_same_type {
// CHECK-NEXT:    method #P1.f: <Self><T where Self == T.A, T : P2> (Self) -> (T) -> () : @$s17witness_same_type2S2VAA2P1A2aDP1fyyqd__1AQyd__RszAA2P2Rd__lFTW
// CHECK-NEXT:  }
