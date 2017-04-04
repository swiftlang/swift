func foo() {
let a = 0; let b = 0 }; unresolved

// Test that offsets of diagnostic ranges and fixits get updated correctly after the edit request

// XFAIL: linux
// RUN: %sourcekitd-test -req=open %s -- %s == -req=print-diags %s \
// RUN:    == -req=edit -pos=2:1 -replace="_" -length=5 %s -print-raw-response \
// RUN: | %FileCheck %s

// CHECK:      key.line: 2,
// CHECK-NEXT: key.column: 5,
// CHECK:      key.description: "initialization of immutable value 'a'
// CHECK:      key.fixits:
// CHECK-NEXT: {
// CHECK-NEXT:   key.offset: 13,
// CHECK-NEXT:   key.length: 5,
// CHECK-NEXT:   key.sourcetext: "_"
// CHECK-NEXT: }

// CHECK:      key.line: 2,
// CHECK-NEXT: key.column: 16,
// CHECK:      key.description: "initialization of immutable value 'b'
// CHECK:      key.fixits:
// CHECK-NEXT: {
// CHECK-NEXT:   key.offset: 24,
// CHECK-NEXT:   key.length: 5,
// CHECK-NEXT:   key.sourcetext: "_"
// CHECK-NEXT: }

// CHECK:      key.line: 2,
// CHECK-NEXT: key.column: 25,
// CHECK:      key.description: "use of unresolved identifier 'unresolved'",
// CHECK:      key.ranges:
// CHECK-NEXT: {
// CHECK-NEXT:   key.offset: 37,
// CHECK-NEXT:   key.length: 10
// CHECK-NEXT: }

// == After the edit =====

// CHECK:      key.line: 2,
// CHECK-NEXT: key.column: 12,
// CHECK:      key.description: "initialization of immutable value 'b'
// CHECK:      key.fixits:
// CHECK-NEXT: {
// CHECK-NEXT:   key.offset: 20,
// CHECK-NEXT:   key.length: 5,
// CHECK-NEXT:   key.sourcetext: "_"
// CHECK-NEXT: }

// CHECK:      key.line: 2,
// CHECK-NEXT: key.column: 21,
// CHECK:      key.description: "use of unresolved identifier 'unresolved'",
// CHECK:      key.ranges:
// CHECK-NEXT: {
// CHECK-NEXT:   key.offset: 33,
// CHECK-NEXT:   key.length: 10
// CHECK-NEXT: }
