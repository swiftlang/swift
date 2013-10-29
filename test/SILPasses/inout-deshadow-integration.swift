// RUN: %swift %s -emit-sil | FileCheck %s

// This is an integration check for the inout-deshadow pass, verifying that it
// deshadows the @inout variables in certain cases.  These test should not be
// very specific (as they are running the parser, silgen and other sil
// diagnostic passes), they should just check the inout shadow got removed.

// CHECK: sil {{.*}}exploded_nontrivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_dead(a : @inout String) {
}

// CHECK: sil {{.*}}exploded_nontrivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_returned(a : @inout String) -> String {
  return a
}
