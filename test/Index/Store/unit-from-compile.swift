// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -index-store-path %t/idx %s -o %t/file1.o -module-name some_module_test
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s
// RUN: %target-swift-frontend -c -index-store-path %t/idx_opt %s -o %t/file1.o -module-name some_module_test -O
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=OPT

// CHECK: file1.o
// CHECK: provider: swift
// CHECK: is-system: 0
// CHECK: is-module: 0
// CHECK: module-name: some_module_test
// CHECK: has-main: 1
// CHECK: main-path: {{.*}}/unit-from-compile.swift
// CHECK: out-file: {{.*}}/file1.o
// CHECK: is-debug: 1

// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}/Swift.swiftmodule
// CHECK: DEPEND END (1)

// OPT: is-debug: 1
