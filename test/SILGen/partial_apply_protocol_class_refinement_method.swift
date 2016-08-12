// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol P { func foo() }
protocol Q: class, P {}

// CHECK-LABEL: sil hidden @_TF46partial_apply_protocol_class_refinement_method12partialApplyFPS_1Q_FT_T_
func partialApply(_ q: Q) -> () -> () {
  // CHECK: [[OPENED:%.*]] = open_existential_ref
  // CHECK: store [[OPENED]] to [[TMP:%.*]] :
  // CHECK: copy_addr [[TMP:%.*]] to [initialization] [[CONSUMABLE_TMP:%.*]] :
  // CHECK: apply {{%.*}}<{{.*}}>([[CONSUMABLE_TMP]])
  return q.foo
}
