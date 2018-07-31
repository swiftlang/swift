// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift -pos=3:3 -length=1 -replace='' == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift -pos=2:1 -replace='' -length=1 == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-remove.swift -pos=1:9 -length=1 -replace='' | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// Initial state

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 20,
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
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 10,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.number,
// CHECK-NEXT:     key.offset: 15,
// CHECK-NEXT:     key.length: 2
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After removing the space before the '56'

// CHECK: {{^}}{
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT: ],

// After deleting the identifier 'l'

// CHECK: {{^}}{
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT: ],

// After deleting the last token on the first line

// CHECK: {{^}}{
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT: ],
