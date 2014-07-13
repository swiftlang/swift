// RUN: %swift %s -emit-sil | FileCheck %s

// This is an integration check for the inout-deshadow pass, verifying that it
// deshadows the inout variables in certain cases.  These test should not be
// very specific (as they are running the parser, silgen and other sil
// diagnostic passes), they should just check the inout shadow got removed.

// CHECK: sil @{{.*}}exploded_trivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_dead(inout a: Int) {
}

// CHECK: sil @{{.*}}exploded_trivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_returned(inout a: Int) -> Int {
  return a
}

// CHECK: sil @{{.*}}exploded_trivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored(inout a: Int) {
  a = 12
}

// CHECK: sil @{{.*}}exploded_trivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored_returned(inout a: Int) -> Int {
  a = 12
  return a
}


// CHECK: sil @{{.*}}exploded_nontrivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_dead(inout a: String) {
}

// CHECK: sil @{{.*}}exploded_nontrivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_returned(inout a: String) -> String {
  return a
}


// CHECK: sil @{{.*}}exploded_nontrivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored(inout a: String) {
  a = "x"
}

// CHECK: sil @{{.*}}exploded_nontrivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored_returned(inout a: String) -> String {
  a = "x"
  return a
}
