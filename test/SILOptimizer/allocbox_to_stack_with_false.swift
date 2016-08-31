// RUN: %target-swift-frontend -emit-sil %s -verify | %FileCheck %s

// Make sure that materializations of computed properties such as 'false' and
// 'true' don't leave needless stack allocations. <rdar://problem/15272642>

// CHECK-LABEL: sil hidden @_TF28allocbox_to_stack_with_false1gFT_T_ : $@convention(thin) () -> () {
// CHECK-NOT: alloc_stack
// CHECK-NOT: dealloc_stack
// CHECK: }
func g() {
  if false {}
}
