// RUN: %target-swift-frontend -emit-silgen %s -swift-version 3 | %FileCheck %s

func fn(_: Any) {}

// CHECK-LABEL: sil hidden @_TF23argument_shuffle_swift31gFT1xP__T_ : $@convention(thin) (@in Any) -> () {
func g(x: Any) {
  // CHECK: [[FN:%.*]] = function_ref @_TF23argument_shuffle_swift32fnFP_T_ : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: 123)
  // CHECK: [[FN:%.*]] = function_ref @_TF23argument_shuffle_swift32fnFP_T_ : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: x)
  // CHECK: return
}
