// RUN: not %swift -Xcc -fake-argument -typecheck %s 2>&1 | %FileCheck %s -check-prefix=CHECK-UNKNOWN-ARG
// CHECK-UNKNOWN-ARG: unknown argument: '-fake-argument'

// RUN: not %swift -Xcc -ivfsoverlay -Xcc %t.nonexistent -typecheck %s 2>&1 | %FileCheck %s -check-prefix=CHECK-VFS-NONEXISTENT
// CHECK-VFS-NONEXISTENT: virtual filesystem overlay file '{{.*}}' not found

// RUN: not %swift -Xcc --version -typecheck %s 2>&1 | %FileCheck %s -check-prefix=CHECK-DRIVER-OPTION
// CHECK-DRIVER-OPTION-DAG: {{clang|LLVM}} version {{[0-9]+\.[0-9]+}}
// CHECK-DRIVER-OPTION-DAG: error: unable to handle compilation, expected exactly one compiler job
// CHECK-DRIVER-OPTION-DAG: error: clang importer creation failed
