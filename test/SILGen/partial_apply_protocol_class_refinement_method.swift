// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

protocol P { func foo() }
protocol Q: class, P {}

// CHECK-LABEL: sil hidden @_T046partial_apply_protocol_class_refinement_method0A5ApplyyycAA1Q_pF
func partialApply(_ q: Q) -> () -> () {
  // CHECK: [[OPENED:%.*]] = open_existential_ref
  // CHECK: [[COPY:%.*]] = copy_value [[OPENED]]
  // CHECK: [[TMP:%.*]] = alloc_stack 
  // CHECK: store [[COPY]] to [init] [[TMP:%.*]] :
  // CHECK: apply {{%.*}}<{{.*}}>([[TMP]])
  // CHECK-NEXT: dealloc_stack [[TMP]]
  return q.foo
}
