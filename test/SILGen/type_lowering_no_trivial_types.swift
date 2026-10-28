// RUN: %target-swift-emit-silgen -Xllvm -type-lowering-no-trivial-types %s | %FileCheck %s

// CHECK-LABEL: sil {{.*}} @$s{{.*}}6borrow{{.*}} : $@convention(thin) (@guaranteed Int) -> ()
func borrow(_: borrowing Int) {}
// CHECK-LABEL: sil {{.*}} @$s{{.*}}7consume{{.*}} : $@convention(thin) (@owned Int) -> ()
func consume(_: consuming Int) {}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}16consume_and_copy{{.*}} : $@convention(thin) (@guaranteed Int) -> ()
// CHECK: bb0(%0 : @guaranteed $Int):
func consume_and_copy(x: Int) {
  // CHECK: [[BORROW:%.*]] = function_ref @$s{{.*}}6borrow
  // CHECK: apply [[BORROW]](%0)
  borrow(x)
  // CHECK: [[COPIED:%.*]] = explicit_copy_value %0
  // CHECK: [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK: apply [[CONSUME]]([[COPIED]])
  consume(copy x)
  // CHECK: [[IMPLICIT_COPIED:%.*]] = copy_value %0
  // CHECK: [[CONSUME:%.*]] = function_ref @$s{{.*}}7consume
  // CHECK: apply [[CONSUME]]([[IMPLICIT_COPIED]])
  consume(x)
}
