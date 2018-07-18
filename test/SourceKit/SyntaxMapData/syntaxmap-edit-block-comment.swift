// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=4:2 -replace=" " -length=1  == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=4:2 -replace="/" -length=1 == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=1:1 -replace="//" -length=2 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s --check-prefixes CHECK,CHECK-OLD
// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=4:2 -replace=" " -length=1 -force-libsyntax-based-processing  == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=4:2 -replace="/" -length=1 -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-block-comment.swift -pos=1:1 -replace="//" -length=2 -force-libsyntax-based-processing | %sed_clean > %t.libSyntax.response
// RUN: %FileCheck -input-file=%t.libSyntax.response %s --check-prefixes CHECK,CHECK-NEW

// Initial state

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 34,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 3,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 7,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 11,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 18,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 20,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 25,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After removing the '/' from the comment close

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 11,
// CHECK-NEXT: key.length: 23,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 11,
// CHECK-NEXT:     key.length: 23
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After adding the '/' back to the comment close

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 11,
// CHECK-NEXT: key.length: 23,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 11,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 18,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 20,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 25,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// after adding a single-line comment on the line above

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-OLD-NEXT: key.length: 3,
// CHECK-NEW-NEXT: key.length: 2,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.comment,
// CHECK-NEXT:     key.offset: 0,
// CHECK-OLD-NEXT:     key.length: 3
// CHECK-NEW-NEXT:     key.length: 2
// CHECK-NEXT:   }
// CHECK-NEXT: ],
