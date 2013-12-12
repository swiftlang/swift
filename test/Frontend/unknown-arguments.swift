// RUN: not %swift_driver -frontend -triple x86_64-apple-darwin10 -fake-argument -abcdef -c %s -o %t.o 2>&1 | FileCheck %s

// CHECK: <unknown>:0: error: unknown argument: '-triple'
// CHECK-NEXT: <unknown>:0: error: unknown argument: '-fake-argument'
// CHECK-NEXT: <unknown>:0: error: unknown argument: '-abcdef'