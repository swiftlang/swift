// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-partial-delete.swift == -req=edit -print-raw-response -pos=2:10 -length=2 -replace='' %S/Inputs/syntaxmap-partial-delete.swift | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 13,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 1,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 5,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 9,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],


// After removing 2 chars from number literal

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 9,
// CHECK-NEXT: key.length: 1,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 9,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ],
