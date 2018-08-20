// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift -pos=10:43 -replace='impact' -length=6 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s

// Original file contents

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 386,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 0,
// CHECK-NEXT:     key.length: 29
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 29,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 33,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-NEXT:     key.offset: 39,
// CHECK-NEXT:     key.length: 9
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 48,
// CHECK-NEXT:     key.length: 28
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 76,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 80,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-NEXT:     key.offset: 86,
// CHECK-NEXT:     key.length: 7
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 93,
// CHECK-NEXT:     key.length: 19
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 112,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 117,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 121,
// CHECK-NEXT:     key.length: 5
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 128,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 136,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 147,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 154,
// CHECK-NEXT:     key.length: 2
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 160,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 164,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 168,
// CHECK-NEXT:     key.length: 51
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 221,
// CHECK-NEXT:     key.length: 35
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 256,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 260,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-NEXT:     key.offset: 266,
// CHECK-NEXT:     key.length: 9
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 275,
// CHECK-NEXT:     key.length: 28
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 303,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 307,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-NEXT:     key.offset: 313,
// CHECK-NEXT:     key.length: 7
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 320,
// CHECK-NEXT:     key.length: 19
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 339,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 344,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 348,
// CHECK-NEXT:     key.length: 5
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 355,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 363,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 374,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 381,
// CHECK-NEXT:     key.length: 2
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After editing a string in between nested comments

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 168,
// CHECK-NEXT: key.length: 51,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 168,
// CHECK-NEXT:     key.length: 51
// CHECK-NEXT:   }
// CHECK-NEXT: ],
