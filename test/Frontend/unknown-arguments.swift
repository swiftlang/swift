// RUN: not %swift -fake-argument -abcdef -c %s -o %t.o 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: error: unknown argument: '-fake-argument'
// CHECK-NEXT: <unknown>:0: error: unknown argument: '-abcdef'

// RUN: not %swiftc_driver -c %s -o %t.o -Xfrontend -fake-frontend-arg -Xfrontend fakevalue 2>&1 | %FileCheck -check-prefix=XFRONTEND %s

// XFRONTEND: <unknown>:0: error: unknown argument: '-fake-frontend-arg'
