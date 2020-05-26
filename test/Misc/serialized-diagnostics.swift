// RUN: rm -f %t.*

// Test swift executable
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck --input-file=%t.deserialized_diagnostics.txt %s

var x = 1 x = 2   // expected-error {{consecutive statements on a line must be separated by ';'}} {{10-10=;}}
// CHECK: {{.*[/\\]}}serialized-diagnostics.swift:[[@LINE-1]]:10: error: consecutive statements on a line must be separated by ';' [] []
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ({{.*[/\\]}}serialized-diagnostics.swift:[[@LINE-3]]:10 - {{.*[/\\]}}serialized-diagnostics.swift:[[@LINE-3]]:10): ";"

var z : Int       // expected-note 2 {{previously declared here}}
var z : Int       // expected-error {{invalid redeclaration}}
// CHECK-NEXT: {{.*[/\\]}}serialized-diagnostics.swift:[[@LINE-1]]:5: error: invalid redeclaration of 'z' [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: +-{{.*[/\\]}}serialized-diagnostics.swift:[[@LINE-4]]:5: note: 'z' previously declared here [] []
// CHECK-NEXT: Number FIXITs = 0

#sourceLocation(file: "fake-file.swuft", line: 4)
var z : Int // Note: no expected-* here because it's "not in this file".
// CHECK-NEXT: {{^}}fake-file.swuft:4:5: error: invalid redeclaration of 'z' [] []
// CHECK-NEXT: Number FIXITs = 0
// CHECK-NEXT: +-{{.*[/\\]}}serialized-diagnostics.swift:{{[0-9]+}}:5: note: 'z' previously declared here [] []
// CHECK-NEXT: Number FIXITs = 0

// CHECK-NEXT: Number of diagnostics: 3
