// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Fooable {
  typealias Bar

  func foo<T: Fooable where T.Bar == Self.Bar>(#x: T) -> Self.Bar
}

struct X {}

// Ensure that the protocol witness for requirements with same-type constraints
// is set correctly. <rdar://problem/16369105>
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17witness_same_type3FooS_7FooableS_FS1_3fooUS1__U__fQPS1_US1___FT1xQ__QS2_3Bar : $@cc(witness_method) @thin <T where T : Fooable, T.Bar == X> (@out X, @in T, @in_guaranteed Foo) -> ()
struct Foo: Fooable {
  typealias Bar = X

  func foo<T: Fooable where T.Bar == X>(#x: T) -> X { return X() }
}

// rdar://problem/19049566
// CHECK-LABEL: sil [transparent] [thunk] @_TTWUSs12SequenceType__USs13GeneratorType__GV17witness_same_type14LazySequenceOfQ_Q0__Ss14_Sequence_TypeS1_FS3_8generateUS3__US0____fQPS3_FT_QS4_9Generator
public struct LazySequenceOf<SS : SequenceType, A where SS.Generator.Element == A> : SequenceType {
	public func generate() -> GeneratorOf<A> { 
    var opt: GeneratorOf<A>?
    return opt!
  }
	public subscript(i : Int) -> A { 
    get { 
      var opt: A?
      return opt!
    } 
  }
}
