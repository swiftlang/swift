// RUN: %target-swift-frontend -emit-silgen %s -swift-version 3 | %FileCheck %s

func fn(_: Any) {}

enum HasAnyCase {
  case any(_: Any)
}

// CHECK-LABEL: sil hidden @_T023argument_shuffle_swift31gyyp1x_tF : $@convention(thin) (@in Any) -> () {
func g(x: Any) {
  // CHECK: [[FN:%.*]] = function_ref @_T023argument_shuffle_swift32fnyypF : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: 123)
  // CHECK: [[FN:%.*]] = function_ref @_T023argument_shuffle_swift32fnyypF : $@convention(thin) (@in Any) -> ()
  // CHECK: apply [[FN:%.*]]({{.*}}) : $@convention(thin) (@in Any) -> ()
  fn(data: x)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(123)

  // CHECK: inject_enum_addr {{.*}} : $*HasAnyCase, #HasAnyCase.any!enumelt.1
  _ = HasAnyCase.any(data: 123)

  // CHECK: return
}
