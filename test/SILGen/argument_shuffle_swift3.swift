
// RUN: %target-swift-frontend -module-name argument_shuffle_swift3 -emit-silgen -enable-sil-ownership %s -swift-version 3 | %FileCheck %s

func fn(_: Any) {}

enum HasAnyCase {
  case any(_: Any)
}

// CHECK-LABEL: sil hidden @$S23argument_shuffle_swift31g1xyyp_tF : $@convention(thin) (@in_guaranteed Any) -> () {
func g(x: Any) {
  // CHECK: [[FN:%.*]] = function_ref @$S23argument_shuffle_swift32fnyyypF : $@convention(thin) (@in_guaranteed Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in_guaranteed Any) -> ()
  fn(data: 123)
  // CHECK: [[FN:%.*]] = function_ref @$S23argument_shuffle_swift32fnyyypF : $@convention(thin) (@in_guaranteed Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in_guaranteed Any) -> ()
  fn(data: x)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(123)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(data: 123)

  // CHECK: return
}
