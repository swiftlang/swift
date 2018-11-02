// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=8:1 -replace='"""' -length=3 == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=6:2 -replace=')' -length=1 == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=2:10 -replace=' ' -length=1 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s --check-prefixes CHECK,CHECK-OLD
// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=8:1 -replace='"""' -length=3 -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=6:2 -replace=')' -length=1 -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-multiline-string.swift -pos=2:10 -replace=' ' -length=1 -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response
// RUN: %FileCheck -input-file=%t.libsyntax.response %s --check-prefixes CHECK,CHECK-NEW

// Original file contents

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 84,
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
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 8,
// CHECK-NEXT:     key.length: 7
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 16,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 20,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 24,
// CHECK-NEXT:     key.length: 60
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After terminating the multiline string

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 24,
// CHECK-NEXT: key.length: 60,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 24,
// CHECK-OLD-NEXT:     key.length: 5
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEW-NEXT:     key.offset: 27,
// CHECK-NEW-NEXT:     key.length: 2
// CHECK-NEW-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string_interpolation_anchor,
// CHECK-NEXT:     key.offset: 30,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 32,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string_interpolation_anchor,
// CHECK-NEXT:     key.offset: 34,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 35,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEW-NEXT:   {
// CHECK-NEW-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEW-NEXT:     key.offset: 38,
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEW-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 43,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 48,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 58,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 69,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 76,
// CHECK-NEXT:     key.length: 5
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After adding a character after the interpolation
// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 35,
// CHECK-OLD-NEXT: key.length: 6,
// CHECK-NEW-NEXT: key.length: 3,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 35,
// CHECK-OLD-NEXT:     key.length: 6
// CHECK-NEW-NEXT:     key.length: 3
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After replacing the middle opening quote with a space

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 24,
// CHECK-NEXT: key.length: 60,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 24,
// CHECK-NEXT:     key.length: 3
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 32,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 38,
// CHECK-NEXT:     key.length: 46
// CHECK-NEXT:   }
// CHECK-NEXT: ],
