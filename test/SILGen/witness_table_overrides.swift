// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// Test the use of "override" on protocol requirements.

protocol P0 {
  func foo()
}

protocol P1 {
  func foo()
}

protocol P2: P1 {
  override func foo()
}

protocol P3: P0, P2 {
  override func foo()
}

// CHECK-LABEL: sil hidden @$S23witness_table_overrides7callFoo1tyx_tAA2P3RzlF
func callFoo<T: P3>(t: T) {
  // CHECK: witness_method $T, #P0.foo!1 : <Self where Self : P0> (Self) -> () -> ()
  t.foo()
}

// CHECK-LABEL: sil_witness_table hidden X3: P3 module witness_table_overrides {
// CHECK-NEXT:    base_protocol P0: X3: P0 module witness_table_overrides
// CHECK-NEXT:    base_protocol P2: X3: P2 module witness_table_overrides
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table hidden X3: P0 module witness_table_overrides {
// CHECK-NEXT:    method #P0.foo!1: <Self where Self : P0> (Self) -> () -> () : @$S23witness_table_overrides2X3VAA2P0A2aDP3fooyyFTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table hidden X3: P2 module witness_table_overrides {
// CHECK-NEXT:    base_protocol P1: X3: P1 module witness_table_overrides
// CHECK-NEXT:  }
struct X3: P3 {
  func foo() { }
}

