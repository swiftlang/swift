// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol Fooable {
  associatedtype Bar

  func foo<T: Fooable where T.Bar == Self.Bar>(x x: T) -> Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17witness_same_type3FooS_7FooableS_FS1_3foo{{.*}} : $@convention(witness_method) <T where T : Fooable, T.Bar == X> (@in T, @in_guaranteed Foo) -> @out X
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
