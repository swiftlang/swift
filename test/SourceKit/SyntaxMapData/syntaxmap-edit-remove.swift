// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift -pos=2:1 -replace='' -length=1 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// Initial state

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 6,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 0,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 4,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After deleting the identifier 'l'

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 4,
// CHECK-NEXT: key.length: 1,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT: ],
