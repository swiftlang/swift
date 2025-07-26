// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -index-store-compress -o %t.o -typecheck %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s

func foo() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | s:4main3fooyyF | Def | rel: 0
