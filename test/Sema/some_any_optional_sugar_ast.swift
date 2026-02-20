// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

protocol P {}
protocol Q {}
struct S: P {}

// CHECK:      (pattern_entry
// CHECK-NEXT:   (pattern_typed type="(any P)?"
// CHECK-NEXT:     (pattern_any type="(any P)?")
// CHECK-NEXT:     (type_optional
// CHECK-NEXT:       (type_existential
// CHECK-NEXT:         (type_unqualified_ident id="P"
let _: any P?

// CHECK:      (pattern_entry
// CHECK-NEXT:   (pattern_typed type="(any Q)?"
// CHECK-NEXT:     (pattern_any type="(any Q)?")
// CHECK-NEXT:     (type_implicitly_unwrapped_optional
// CHECK-NEXT:       (type_existential
// CHECK-NEXT:         (type_unqualified_ident id="Q"
let _: any Q!

// CHECK:      (pattern_entry
// CHECK-NEXT:   (pattern_typed type="(any P & Q)?"
// CHECK-NEXT:     (pattern_any type="(any P & Q)?")
// CHECK-NEXT:     (type_optional
// CHECK-NEXT:       (type_existential
// CHECK-NEXT:         (type_tuple
// CHECK-NEXT:           (type_composite
// CHECK-NEXT:             (type_unqualified_ident id="P"
// CHECK-NEXT:             (type_unqualified_ident id="Q"
let _: any (P & Q)?

// CHECK:      (func_decl {{.*}}result="(some P)?" 
// CHECK-NEXT:   (opaque_result_decl=opaque_type {{.*}}interface_type="(some P)?.Type"
func f() -> some P? { S() }

// CHECK:      (func_decl {{.*}}interface_type=" ((some P)?) -> ()"
func g(_: some P?) {}
