// RUN: %target-swift-frontend -emit-sil -verify -sil-verify-all %s | %FileCheck %s

protocol P {
  func method()
}

// CHECK-LABEL: sil {{.*}}invoke
// CHECK: witness_method $T.Type, #P.method
func invoke<T>(_: T.Type) where T.Type: P {
  T.method()
}
