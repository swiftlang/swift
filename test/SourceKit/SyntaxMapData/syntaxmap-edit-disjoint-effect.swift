// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-disjoint-effect.swift == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-disjoint-effect.swift -pos=1:11 -replace='(' -length=1 | %sed_clean > %t.response
// RUN: %FileCheck -input-file=%t.response %s
// RUN: %sourcekitd-test -req=open -print-raw-response %S/Inputs/syntaxmap-edit-disjoint-effect.swift -force-libsyntax-based-processing == -req=edit -print-raw-response %S/Inputs/syntaxmap-edit-disjoint-effect.swift -pos=1:11 -replace='(' -length=1 -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response
// RUN: %FileCheck -input-file=%t.libsyntax.response %s

// Original contents

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 0,
// CHECK-NEXT: key.length: 86,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 0,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 5,
// CHECK-NEXT:     key.length: 5
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 12,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 19,
// CHECK-NEXT:     key.length: 8
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 32,
// CHECK-NEXT:     key.length: 4
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 37,
// CHECK-NEXT:     key.length: 14
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.typeidentifier,
// CHECK-NEXT:     key.offset: 57,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.keyword,
// CHECK-NEXT:     key.offset: 70,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.string,
// CHECK-NEXT:     key.offset: 77,
// CHECK-NEXT:     key.length: 2
// CHECK-NEXT:   }
// CHECK-NEXT: ],

// After opening the parameter list (without closing it)

// CHECK: {{^}}{
// CHECK-NEXT: key.offset: 12,
// CHECK-NEXT: key.length: 51,
// CHECK-NEXT: key.diagnostic_stage: source.diagnostic.stage.swift.parse,
// CHECK-NEXT: key.syntaxmap: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.syntaxtype.identifier,
// CHECK-NEXT:     key.offset: 12,
// CHECK-NEXT:     key.length: 6
// CHECK-NEXT:   }

