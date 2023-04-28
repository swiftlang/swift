// RUN: %sourcekitd-test -req=diags %s -- %s -I %S/../Inputs/header_with_macro | %FileCheck %s

import HeaderWithMacro

_ = FOO(5)

// CHECK:      key.diagnostics: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.line: 5,
// CHECK-NEXT:     key.column: 5,
// CHECK-NEXT:     key.filepath: "{{.*}}/rdar107281079.swift",
// CHECK-NEXT:     key.severity: source.diagnostic.severity.error,
// CHECK-NEXT:     key.id: "cannot_find_in_scope",
// CHECK-NEXT:     key.description: "cannot find 'FOO' in scope",
// CHECK-NEXT:     key.ranges: [
// CHECK-NEXT:       {
// CHECK-NEXT:         key.offset: 124,
// CHECK-NEXT:         key.length: 3
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     key.diagnostics: [
// CHECK-NEXT:       {
// CHECK-NEXT:         key.line: 4,
// CHECK-NEXT:         key.column: 9,
// CHECK-NEXT:         key.filepath: "{{.*}}/Inputs/header_with_macro/Header.h",
// CHECK-NEXT:         key.severity: source.diagnostic.severity.note,
// CHECK-NEXT:         key.id: "macro_not_imported_function_like",
// CHECK-NEXT:         key.description: "macro 'FOO' unavailable: function like macros not supported"
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   }
// CHECK-NEXT: ]
