// RUN: rm -f %t.*

// Test swift executable
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck --input-file=%t.deserialized_diagnostics.txt %s

// Test swift_driver integrated frontend
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.integrated_frontend.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.integrated_frontend.dia > %t.integrated_frontend.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck --input-file=%t.integrated_frontend.deserialized_diagnostics.txt %s

var x = 1 x = 2   // expected-error {{consecutive statements on a line must be separated by ';'}} {{10-10=;}}
var z : Int       // expected-note {{previously declared here}}
var z : Int       // expected-error {{invalid redeclaration}}

// CHECK: 13:10: error: consecutive statements on a line must be separated by ';' [] []
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ({{.*[/\\]}}serialized-diagnostics.swift:13:10 - {{.*[/\\]}}serialized-diagnostics.swift:13:10): ";"
// CHECK-NEXT: 15:5: error: invalid redeclaration of 'z' [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: +-{{.*[/\\]}}serialized-diagnostics.swift:14:5: note: 'z' previously declared here [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: Number of diagnostics: 2
