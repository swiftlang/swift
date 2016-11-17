// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

protocol Fooable {
  associatedtype Bar

  func foo<T: Fooable where T.Bar == Self.Bar>(x x: T) -> Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17witness_same_type3FooS_7FooableS_FS1_3foouRd__S1_wx3Barzwd__S2_rfT1xqd___wxS2_ : $@convention(witness_method) <τ_0_0 where τ_0_0 : Fooable, τ_0_0.Bar == X> (@in τ_0_0, @in_guaranteed Foo) -> @out X
struct Foo: Fooable {
  typealias Bar = X

  func foo<T: Fooable where T.Bar == X>(x x: T) -> X { return X() }
}

// rdar://problem/19049566
// CHECK-LABEL: sil [transparent] [thunk] @_TTWu0_Rxs8Sequence_zWx8Iterator7Element_rGV17witness_same_type14LazySequenceOfxq__S_S2_FS_12makeIterator
public struct LazySequenceOf<SS : Sequence, A where SS.Iterator.Element == A> : Sequence {
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
