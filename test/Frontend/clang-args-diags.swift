// RUN: not %swift -Xcc -fake-argument -parse %s 2>&1 | FileCheck %s -check-prefix=CHECK-UNKNOWN-ARG
// CHECK-UNKNOWN-ARG: unknown argument: '-fake-argument'

// RUN: not %swift -Xcc -ivfsoverlay -Xcc %t.nonexistent -parse %s 2>&1 | FileCheck %s -check-prefix=CHECK-VFS-NONEXISTENT
// CHECK-VFS-NONEXISTENT: virtual filesystem overlay file '{{.*}}' not found
