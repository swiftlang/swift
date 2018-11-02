// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift -pos=10:43 -replace='impact' -length=6 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s --check-prefixes CHECK,CHECK-OLD
// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-nested-token.swift -pos=10:43 -replace='impact' -length=6 -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response
// RUN: %FileCheck -input-file=%t.libsyntax.response %s --check-prefixes CHECK,CHECK-NEW

// Original file contents

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 386,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 0,
// CHECK-OLD-NEXT:     key.length: 29
// CHECK-NEW-NEXT:     key.length: 28
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 29,
// CHECK-OLD-NEXT:     key.length: 4
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 33,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-OLD-NEXT:     key.offset: 39,
// CHECK-OLD-NEXT:     key.length: 9
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 48,
// CHECK-OLD-NEXT:     key.length: 28
// CHECK-OLD-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEW-NEXT:     key.offset: 33,
// CHECK-NEW-NEXT:     key.length: 42
// CHECK-NEW-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 76,
// CHECK-OLD-NEXT:     key.length: 4
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 80,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-OLD-NEXT:     key.offset: 86,
// CHECK-OLD-NEXT:     key.length: 7
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 93,
// CHECK-OLD-NEXT:     key.length: 19
// CHECK-OLD-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEW-NEXT:     key.offset: 80,
// CHECK-NEW-NEXT:     key.length: 31
// CHECK-NEW-NEXT:   },
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
// CHECK-OLD-NEXT:     key.length: 35
// CHECK-NEW-NEXT:     key.length: 34
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 256,
// CHECK-OLD-NEXT:     key.length: 4
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 260,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-OLD-NEXT:     key.offset: 266,
// CHECK-OLD-NEXT:     key.length: 9
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 275,
// CHECK-OLD-NEXT:     key.length: 28
// CHECK-OLD-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEW-NEXT:     key.offset: 260,
// CHECK-NEW-NEXT:     key.length: 42
// CHECK-NEW-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEXT:     key.offset: 303,
// CHECK-OLD-NEXT:     key.length: 4
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 307,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment.field,
// CHECK-OLD-NEXT:     key.offset: 313,
// CHECK-OLD-NEXT:     key.length: 7
// CHECK-OLD-NEXT:   },
// CHECK-OLD-NEXT:   {
// CHECK-OLD-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-OLD-NEXT:     key.offset: 320,
// CHECK-OLD-NEXT:     key.length: 19
// CHECK-OLD-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.doccomment,
// CHECK-NEW-NEXT:     key.offset: 307,
// CHECK-NEW-NEXT:     key.length: 31
// CHECK-NEW-NEXT:   },
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
