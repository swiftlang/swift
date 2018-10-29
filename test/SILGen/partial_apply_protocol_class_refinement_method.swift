
// RUN: %target-swift-emit-silgen -module-name partial_apply_protocol_class_refinement_method -enable-sil-ownership %s | %FileCheck %s

protocol P { func foo() }
protocol Q: class, P {}

// CHECK-LABEL: sil hidden @$s46partial_apply_protocol_class_refinement_method0A5ApplyyyycAA1Q_pF : $@convention
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Q):
func partialApply(_ q: Q) -> () -> () {
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[ARG]]
  // CHECK: [[TMP:%.*]] = alloc_stack 
  // CHECK: store_borrow [[OPENED]] to [[TMP:%.*]] :
  // CHECK: apply {{%.*}}<{{.*}}>([[TMP]])
  // CHECK-NEXT: dealloc_stack [[TMP]]
  return q.foo
}
