// RUN: rm -f %t.*

// Test swift executable
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics-path %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck --input-file=%t.deserialized_diagnostics.txt %s

var x = String.init // expected-error{{ambiguous use of 'init'}}
// CHECK: {{.*[/\\]}}serialized-diagnostics-prettyprint.swift:[[@LINE-1]]:16: error: ambiguous use of 'init'

// CHECK: Swift.String:2:23: note: found this candidate
// CHECK: CONTENTS OF FILE Swift.String:
// CHECK: extension String {
// CHECK:    public init(_ content: Substring.UnicodeScalarView)
