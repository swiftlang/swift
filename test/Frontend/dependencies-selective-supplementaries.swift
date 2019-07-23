// This test verifies that, in batch-mode (i.e. >1 primary input),
// we have fixed a bug that caused every primary input to be a dependent
// of every ModuleOutputPath, etc.

// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func main() {a()}' >%t/main.swift
// RUN: %target-swift-frontend -c -enable-batch-mode -module-name foo -primary-file %t/a.swift -primary-file %t/main.swift -emit-dependencies-path %t/a.d -emit-dependencies-path %t/main.d  -o %t/a.o -o %t/main.o -emit-module-path %t/a.swiftmodule -emit-module-path %t/main.swiftmodule
// RUN: %FileCheck -check-prefix=CHECK-MAIN %s <%t/main.d
// RUN: %FileCheck -check-prefix=NEGATIVE-MAIN %s <%t/main.d
//
// CHECK-MAIN-DAG: main.swiftmodule :
// CHECK-MAIN-DAG: main.o :
// NEGATIVE-MAIN-NOT: a.swiftmodule
// NEGATIVE-MAIN-NOT: a.o
//
// RUN: %FileCheck -check-prefix=CHECK-A %s <%t/a.d
// RUN: %FileCheck -check-prefix=NEGATIVE-A %s <%t/a.d
//
// CHECK-A-DAG: a.swiftmodule :
// CHECK-A-DAG: a.o :
// NEGATIVE-A-NOT: main.swiftmodule
// NEGATIVE-A-NOT: main.o
