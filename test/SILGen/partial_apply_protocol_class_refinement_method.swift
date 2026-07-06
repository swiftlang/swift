
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name partial_apply_protocol_class_refinement_method %s | %FileCheck %s

protocol P { func foo() }
protocol Q: class, P {}

// CHECK-LABEL: sil hidden [ossa] @$s46partial_apply_protocol_class_refinement_method0A5ApplyyyycAA1Q_pF : $@convention
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Q):
func partialApply(_ q: Q) -> () -> () {
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[ARG]]
  // CHECK: [[TMP:%.*]] = alloc_stack 
  // CHECK: [[SB:%.*]] = store_borrow [[OPENED]] to [[TMP:%.*]] :
  // CHECK: apply {{%.*}}<{{.*}}>([[SB]])
  // CHECK: end_borrow
  // CHECK-NEXT: dealloc_stack [[TMP]]
  return q.foo
}
