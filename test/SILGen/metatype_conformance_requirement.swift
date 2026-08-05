// RUN: %target-swift-frontend -emit-sil -verify -sil-verify-all %s | %FileCheck %s

protocol P {
  func method()
}

struct Storage<Value> {
  var value: Value
}

struct Outer<T> where T.Type: P {
  struct Nested {
    var value: T?
  }

  // Forming Nested's context substitution map must preserve the abstract
  // T.Type: P conformance inherited from Outer.
  var storage: Storage<Nested>
}

// CHECK-LABEL: sil {{.*}}invoke
// CHECK: witness_method $T.Type, #P.method
func invoke<T>(_: T.Type) where T.Type: P {
  T.method()
}
