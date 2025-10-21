// REQUIRES: shell

// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx -index-store-compress -o %t.o -typecheck %s
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s

// Check that the unit file starts with CIDXR, indicating that the record is indeed compressed.
// RUN: head -1 %t/idx/**/records/**/*index_compress.swift* | grep '^CIDXR'

func foo() {}
// CHECK: [[@LINE-1]]:6 | function/Swift | s:4main3fooyyF | Def | rel: 0
