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
