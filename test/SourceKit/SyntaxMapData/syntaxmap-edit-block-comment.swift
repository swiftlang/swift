// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=3:2 -replace=" " -length=1  == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=3:2 -replace="/" -length=1 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 29,
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
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 15,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 20,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 8,
// CHECK-NEXT: key.length: 21,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 21
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 8,
// CHECK-NEXT: key.length: 21,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 15,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 20,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],

