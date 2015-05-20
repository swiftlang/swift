// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable {
  typealias Bar

  func foo<T: Fooable where T.Bar == Self.Bar>(x x: T) -> Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17witness_same_type3FooS_7FooableS_FS1_3foou__Rq_S1_qd__S1_zqq_S1_3Barqqd__S1_3Bar_fq_FT1xqd___qq_S1_3Bar : $@convention(witness_method) <T where T : Fooable, T.Bar == X> (@out X, @in T, @in_guaranteed Foo) -> ()
struct Foo: Fooable {
  typealias Bar = X

  func foo<T: Fooable where T.Bar == X>(x x: T) -> X { return X() }
}

// rdar://problem/19049566
// CHECK-LABEL: sil [transparent] [thunk] @_TTWu0_Rq_Ss12SequenceTypezq0_qqq_S_9GeneratorSs13GeneratorType7Element_GV17witness_same_type14LazySequenceOfq_q0__Ss14_Sequence_TypeS1_FS3_8generateuRq_S3__fq_FT_qq_S3_9Generator
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
