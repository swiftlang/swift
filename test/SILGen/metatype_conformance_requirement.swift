// RUN: %target-swift-frontend -emit-sil -verify -sil-verify-all %s | %FileCheck %s

protocol P {
  func method()
}

// CHECK-LABEL: sil {{.*}}invoke
// CHECK: witness_method $T.Type, #P.method
func invoke<T>(_: T.Type) where T.Type: P {
  T.method()
}

func receive<each T>(_: repeat (each T).Type)
    where repeat (each T).Type: P {
}

// CHECK-LABEL: sil {{.*}}forward
// CHECK: apply {{.*}}<Pack{repeat each T}>
func forward<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  receive(repeat each types)
}
