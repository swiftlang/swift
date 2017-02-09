// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-sil | %FileCheck %s

// This is an integration check for the inout-deshadow pass, verifying that it
// deshadows the inout variables in certain cases.  These test should not be
// very specific (as they are running the parser, silgen and other sil
// diagnostic passes), they should just check the inout shadow got removed.

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_dead(a: inout Int) {
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_returned(a: inout Int) -> Int {
  return a
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored(a: inout Int) {
  a = 12
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_trivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_trivial_type_stored_returned(a: inout Int) -> Int {
  a = 12
  return a
}


// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_dead
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_dead(a: inout String) {
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_returned(a: inout String) -> String {
  return a
}


// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_stored
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored(a: inout String) {
  a = "x"
}

// CHECK-LABEL: sil hidden @{{.*}}exploded_nontrivial_type_stored_returned
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }
func exploded_nontrivial_type_stored_returned(a: inout String) -> String {
  a = "x"
  return a
}


// Use an external function so inout deshadowing cannot see its body.
@_silgen_name("takesNoEscapeClosure")
func takesNoEscapeClosure(fn : () -> Int)

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

// CHECK-LABEL: sil hidden @_T026inout_deshadow_integration24StructWithMutatingMethodV08mutatingG0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout StructWithMutatingMethod) -> () {
// CHECK-NOT: alloc_box
// CHECK-NOT: alloc_stack
// CHECK: }

// CHECK-LABEL: sil hidden @_T026inout_deshadow_integration24StructWithMutatingMethodV28testStandardLibraryOperators{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout StructWithMutatingMethod) -> () {
// CHECK-NOT: alloc_box $<τ_0_0> { var τ_0_0 } <StructWithMutatingMethod>
// CHECK-NOT: alloc_stack $StructWithMutatingMethod
// CHECK: }


