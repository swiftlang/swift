// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -c -swift-version 5 -index-store-path %t/idx %s -o %t/file1.o -module-name some_module_test
// RUN: %target-swift-frontend -version > %t/unit.out
// RUN: c-index-test core -print-unit %t/idx >> %t/unit.out
// RUN: %FileCheck %s < %t/unit.out
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -c -index-store-path %t/idx_opt %s -o %t/file1.o -module-name some_module_test -O
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=OPT

// CHECK: [[COMPILER_VERSION:.*Swift version.*]]
// CHECK: file1.o
// CHECK: provider: swift-[[COMPILER_VERSION]]{{$}}
// CHECK: is-system: 0
// CHECK: is-module: 0
// CHECK: module-name: some_module_test
// CHECK: has-main: 1
// CHECK: main-path: {{.*}}{{/|\\}}unit-from-compile.swift
// CHECK: out-file: {{.*}}{{/|\\}}file1.o
// CHECK: is-debug: 1

// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}{{/|\\}}Swift.swiftmodule
// CHECK: Record | user | {{.*}}{{/|\\}}unit-from-compile.swift
// CHECK: DEPEND END (2)

// OPT: is-debug: 1
