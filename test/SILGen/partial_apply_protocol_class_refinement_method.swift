// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol P { func foo() }
protocol Q: class, P {}

// CHECK-LABEL: sil hidden @_T046partial_apply_protocol_class_refinement_method0A5ApplyyycAA1Q_pF
func partialApply(_ q: Q) -> () -> () {
  // CHECK: [[OPENED:%.*]] = open_existential_ref
  // CHECK: store [[OPENED]] to [init] [[TMP:%.*]] :
  // CHECK: copy_addr [[TMP:%.*]] to [initialization] [[CONSUMABLE_TMP:%.*]] :
  // CHECK: apply {{%.*}}<{{.*}}>([[CONSUMABLE_TMP]])
  return q.foo
}
