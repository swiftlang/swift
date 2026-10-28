// RUN: %target-swift-emit-silgen -Xllvm -silgen-ownership-for-trivial %s | %FileCheck %s

// CHECK-LABEL: sil {{.*}} [ownership_for_trivial] {{.*}} @$s{{.*}}6borrow{{.*}} : $@convention(thin) (Int) -> ()
// CHECK: bb0(%0 : @noImplicitCopy @guaranteed $Int):
// CHECK-NOT:  destroy_value %0
// CHECK:      return
func borrow(_: borrowing Int) {
}

// CHECK-LABEL: sil {{.*}} [ownership_for_trivial] {{.*}} @$s{{.*}}7consume{{.*}} : $@convention(thin) (Int) -> ()
// CHECK: bb0(%0 : @noImplicitCopy @_eagerMove @owned $Int):
// CHECK:   destroy_value %0
// CHECK:   return
func consume(_: consuming Int) {
}

// CHECK-LABEL: sil {{.*}} [ownership_for_trivial] {{.*}} @$s{{.*}}16consume_and_copy{{.*}} : $@convention(thin) (Int) -> ()
// CHECK: bb0(%0 : @guaranteed $Int):
func consume_and_copy(x: Int) {
  // CHECK: [[BORROW:%.*]] = function_ref @$s{{.*}}6borrow
  // CHECK: apply [[BORROW]](%0)
  borrow(x)
  // CHECK: [[COPIED:%.*]] = explicit_copy_value %0
  // CHECK: [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // A function doesn't outwardly consume trivial values.
  // CHECK: apply [[CONSUME]]([[COPIED]])
  consume(copy x)
  // CHECK: [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK: apply [[CONSUME]](%0)
  consume(x)
}
