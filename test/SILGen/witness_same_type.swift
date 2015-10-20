// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable {
  typealias Bar

  func foo<T: Fooable where T.Bar == Self.Bar>(x x: T) -> Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17witness_same_type3FooS_7FooableS_FS1_3foou__R_S1_d__S1_w_3Barzwd__S2_rfq_FT1xqd___w_S2_ : $@convention(witness_method) <T where T : Fooable, T.Bar == X> (@out X, @in T, @in_guaranteed Foo) -> ()
struct Foo: Fooable {
  typealias Bar = X

  func foo<T: Fooable where T.Bar == X>(x x: T) -> X { return X() }
}

// rdar://problem/19049566
// CHECK-LABEL: sil [transparent] [thunk] @_TTWu0_R_s12SequenceType0_zW_9Generator7Element_rGV17witness_same_type14LazySequenceOfq_q0__S_S2_FS_8generateuR_S_rfq_FT_w_S0_
public struct LazySequenceOf<SS : SequenceType, A where SS.Generator.Element == A> : SequenceType {
	public func generate() -> AnyGenerator<A> { 
    var opt: AnyGenerator<A>?
    return opt!
  }
	public subscript(i : Int) -> A { 
    get { 
      var opt: A?
      return opt!
    } 
  }
}
