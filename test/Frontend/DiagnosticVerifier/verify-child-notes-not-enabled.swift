// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not warning: --implicit-check-not note:

struct A {}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: unexpected note produced: 'A' previously declared here
struct A {} // expected-error {{invalid redeclaration of 'A'}} {{children: expected-note@-1 {{'A' previously declared here}} }}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: child diagnostics block requires -verify-child-notes

struct B {} // expected-note {{'B' previously declared here}}
struct B {} // expected-error {{invalid redeclaration of 'B'}} {{children:  }}
// CHECK: [[@LINE-1]]:{{[0-9]+}}: error: child diagnostics block requires -verify-child-notes

