// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

struct G<T> {}

// CHECK-LABEL: Generic signature: <T where T == Error>
extension G where T : Error, T == Error {}
// expected-warning@-1 {{redundant conformance constraint 'T' : 'Error'}}
// expected-note@-2 {{conformance constraint 'T' : 'Error' implied here}}