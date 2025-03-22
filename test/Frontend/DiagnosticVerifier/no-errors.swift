// RUN: not %target-swift-frontend -parse -verify=no-errors %s 2>&1 | %FileCheck %s

// CHECK-NOT: error:

// CHECK: [[@LINE+1]]:13: error: verification options are set to disallow error expectations
class {} // expected-error {{expected identifier in class declaration}}
// CHECK-NOT: error:
// CHECK: [[@LINE+2]]:13: error: verification options are set to disallow error expectations
// CHECK-NOT: error:
class {} // expected-error {{expected identifier in struct declaration}}
// CHECK: [[@LINE-1]]:30: error: incorrect message found
// CHECK-NOT: error:
// CHECK: [[@LINE+1]]:7: error: unexpected error produced: expected identifier in class declaration
class {}

// CHECK-NOT: error:
