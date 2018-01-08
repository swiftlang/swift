// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -module-name argument_shuffle_swift3 -emit-silgen -enable-sil-ownership %s -swift-version 3 | %FileCheck %s

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

  // CHECK:      // function_ref HasAnyCase.any(_:)
  // CHECK-NEXT: [[ENUM_CASE:%.*]] = function_ref @$S23argument_shuffle_swift310HasAnyCaseO3anyyACypcACmF
  // CHECK-NEXT: apply [[ENUM_CASE]]({{.*}})
  _ = HasAnyCase.any(123)

  // CHECK:      // function_ref HasAnyCase.any(_:)
  // CHECK-NEXT: [[ENUM_CASE:%.*]] = function_ref @$S23argument_shuffle_swift310HasAnyCaseO3anyyACypcACmF
  // CHECK-NEXT: apply [[ENUM_CASE]]({{.*}})
  _ = HasAnyCase.any(data: 123)

  // CHECK: return
}
