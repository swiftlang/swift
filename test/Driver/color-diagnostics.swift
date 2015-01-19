// RUN: not %target-swiftc_driver -Xfrontend -color-diagnostics -emit-executable -o %t %s 2>&1 | FileCheck %s

// CHECK: [0m1 = 2{{$}}
1 = 2
