// RUN: rm -f %t.*
// RUN: %swift -parse --serialize-diagnostics %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: FileCheck --input-file=%t.deserialized_diagnostics.txt %s

var x = 1 x = 2   // expected-error {{consecutive statements on a line must be separated by ';'}}
var z : Int       // expected-note {{'z' previously declared here}}
var z : Int       // expected-error {{invalid redeclaration}}

// CHECK: 6:10: error: consecutive statements on a line must be separated by ';' [] []
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ({{.*[/\\]}}serialized-diagnostics.swift:6:10 - {{.*[/\\]}}serialized-diagnostics.swift:6:10): ";"
// CHECK-NEXT: 8:5: error: invalid redeclaration [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: +-{{.*[/\\]}}serialized-diagnostics.swift:7:5: note: 'z' previously declared here [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: Number of diagnostics: 2
