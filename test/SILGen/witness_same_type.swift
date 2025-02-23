
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
