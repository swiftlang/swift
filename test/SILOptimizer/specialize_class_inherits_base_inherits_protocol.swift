// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

protocol P { func p() -> Any.Type }
protocol Q: P { }

@inline(never) func sink<T>(_ x: T) {}

func p<T: Q>(_ x: T) { sink(x.p()) }

class Foo<T>: Q { func p() -> Any.Type { return T.self } }
class Bar<T>: Foo<T> {}

// CHECK-LABEL: sil @_TF48specialize_class_inherits_base_inherits_protocol3fooFT_T_
public func foo() {
  // CHECK: function_ref @_TTSf4d___TTSg5PMP____TF48specialize_class_inherits_base_inherits_protocol4sinkurFxT_
  p(Bar<Int>())
}

