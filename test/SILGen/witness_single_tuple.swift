// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol Runcible {
  func runce(x: Int)
}

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV20witness_single_tuple3FooS_8RuncibleS_FS1_5runceuRq_S1__fq_FSiT_
struct Foo: Runcible {
  func runce(x: Int = 0) {}
}

// Force a reabstraction in the other direction, where the "orig" pattern is
// a non-tuple and the input is a labeled single-element tuple.
func labeledArgToAny() -> Any {
  func labeledFunc(f f: Int -> ()) -> Int { return 0 }
  // FIXME: labeled tuple with tuple still broken; rdar://problem/21496105
  // func labeledTupleWithFunc(f f: (Int -> (), Int -> ())) -> Int { return 0 }
  if true {
    return labeledFunc
  } /*else {
    return labeledTupleWithFunc
  }*/
}
