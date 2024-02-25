// RUN: not %target-swiftc_driver -color-diagnostics -diagnostic-style=llvm -emit-executable -o %t %s 2>&1 \
// RUN:     | %FileCheck -check-prefix=CHECK-CD %s
// CHECK-CD: [0m1 = 2{{$}}

// RUN: not %target-swiftc_driver -no-color-diagnostics -emit-executable -o %t %s 2>&1 \
// RUN:     | %FileCheck -check-prefix=CHECK-NCD %s
// CHECK-NCD: {{[ ]}}1 = 2

1 = 2
