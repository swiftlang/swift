// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-chained-comment.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-chained-comment.swift -pos=1:9 -replace=" " -length=1  == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-chained-comment.swift -pos=1:9 -replace="/" -length=1 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// Initial state

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 25,
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
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 16,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 23,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ],


// After replacing the '/' from the comment open with ' '

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 8,
// CHECK-NEXT: key.length: 14,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 13,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After adding the '/' back to the comment open

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 8,
// CHECK-NEXT: key.length: 14,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 16,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   }
// CHECK-NEXT: ],
