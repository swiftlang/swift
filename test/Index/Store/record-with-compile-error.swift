// XFAIL: linux

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck %s -verify
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s

// CHECK: function/Swift | test1() | [[TEST1_FUNC:.*]] | <no-cgname> | Def
// CHECK: [[@LINE+1]]:6 | function/Swift | [[TEST1_FUNC]]
func test1() {
  unresolved() // expected-error {{use of unresolved identifier}}
}
