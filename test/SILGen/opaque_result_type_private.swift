// RUN: %target-swift-emit-silgen -primary-file %s -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-emit-sil -primary-file %s -O -disable-availability-checking

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {

// CHECK: [[BOX:%.*]] = alloc_stack $PrivateClass
// CHECK: [[FN:%.*]] = function_ref @$s26opaque_result_type_private19returnPrivateOpaqueQryF : $@convention(thin) () -> @out PrivateClass
// CHECK: apply [[FN]]([[BOX]]) : $@convention(thin) () -> @out PrivateClass
// CHECK: [[RESULT:%.*]] = load [take] [[BOX]] : $*PrivateClass
// CHECK: destroy_value [[RESULT]] : $PrivateClass
// CHECK: dealloc_stack [[BOX]] : $*PrivateClass
_ = returnPrivateOpaque()

// CHECK: [[BOX:%.*]] = alloc_stack $LocalClass
// CHECK: [[FN:%.*]] = function_ref @$s26opaque_result_type_private17returnLocalOpaqueQryF : $@convention(thin) () -> @out LocalClass
// CHECK: apply [[FN]]([[BOX]]) : $@convention(thin) () -> @out LocalClass
// CHECK: [[RESULT:%.*]] = load [take] [[BOX]] : $*LocalClass
// CHECK: destroy_value [[RESULT]] : $LocalClass
// CHECK: dealloc_stack [[BOX]] : $*LocalClass
_ = returnLocalOpaque()

fileprivate class PrivateClass {}

// CHECK-LABEL: sil hidden [ossa] @$s26opaque_result_type_private19returnPrivateOpaqueQryF : $@convention(thin) () -> @out @_opaqueReturnTypeOf("$s26opaque_result_type_private19returnPrivateOpaqueQryF", 0) 🦸
func returnPrivateOpaque() -> some Any {
  return PrivateClass()
}

// CHECK-LABEL: sil hidden [ossa] @$s26opaque_result_type_private17returnLocalOpaqueQryF : $@convention(thin) () -> @out @_opaqueReturnTypeOf("$s26opaque_result_type_private17returnLocalOpaqueQryF", 0) 🦸
func returnLocalOpaque() -> some Any {
  class LocalClass {}

  return LocalClass()
}
