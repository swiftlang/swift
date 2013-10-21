// RUN: %swift -emit-sil %s | FileCheck %s

// Make sure that materializations of computed properties such as 'false' and
// 'true' don't leave needless stack allocations. <rdar://problem/15272642>

// CHECK-LABEL: sil @_T28allocbox_to_stack_with_false1gFT_T_ : $@thin () -> () {
// CHECK-NOT: alloc_stack
// CHECK-NOT: dealloc_stack
// CHECK: }
func g() {
  if false {}
}

// CHECK-LABEL: sil deserialized
