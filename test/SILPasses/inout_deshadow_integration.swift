// RUN: %target-swift-frontend %s -emit-sil | FileCheck %s

// This is an integration check for the inout-deshadow pass, verifying that it
// deshadows the inout variables in certain cases.  These test should not be
// very specific (as they are running the parser, silgen and other sil
// diagnostic passes), they should just check the inout shadow got removed.

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_dead(inout a: Int) {
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_returned(inout a: Int) -> Int {
  return a
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored(inout a: Int) {
  a = 12
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored_returned(inout a: Int) -> Int {
  a = 12
  return a
}


// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_dead(inout a: String) {
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_returned(inout a: String) -> String {
  return a
}


// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored(inout a: String) {
  a = "x"
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored_returned(inout a: String) -> String {
  a = "x"
  return a
}


// Use an external function so inout deshadowing cannot see its body.
@asmname("takesNoEscapeClosure")
func takesNoEscapeClosure(@noescape fn : () -> Int)

struct StructWithMutatingMethod {
  var x = 42

  // We should be able to deshadow this.  The closure is capturing self, but it
  // is itself noescape.
  mutating func mutatingMethod() {
    takesNoEscapeClosure { x = 42; return x }
  }

  mutating func testStandardLibraryOperators() {
    if x != 4 || x != 0 {
      testStandardLibraryOperators()
    }
  }
}

// CHECK-LABEL: sil hidden @_TFV26inout_deshadow_integration24StructWithMutatingMethod14mutatingMethodfRS0_FT_T_ : $@convention(method) (@inout StructWithMutatingMethod) -> () {
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }

// CHECK-LABEL: sil hidden @_TFV26inout_deshadow_integration24StructWithMutatingMethod28testStandardLibraryOperatorsfRS0_FT_T_ : $@convention(method) (@inout StructWithMutatingMethod) -> () {
// CHECK-NOT: alloc_box $StructWithMutatingMethod
// CHECK-NOT: alloc_stack $StructWithMutatingMethod
// CHECK: }


