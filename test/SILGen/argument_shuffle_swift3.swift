// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s -swift-version 3 | %FileCheck %s

func fn(_: Any) {}

enum HasAnyCase {
  case any(_: Any)
}

// CHECK-LABEL: sil hidden @$S23argument_shuffle_swift31g1xyyp_tF : $@convention(thin) (@in Any) -> () {
func g(x: Any) {
  // CHECK: [[FN:%.*]] = function_ref @$S23argument_shuffle_swift32fnyyypF : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: 123)
  // CHECK: [[FN:%.*]] = function_ref @$S23argument_shuffle_swift32fnyyypF : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: x)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(123)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(data: 123)

  // CHECK: return
}
