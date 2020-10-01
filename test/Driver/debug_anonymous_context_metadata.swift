// RUN: %target-swiftc_driver -### -g %s 2>&1 | %FileCheck %s
// RUN: %target-swiftc_driver -### -g -Onone %s 2>&1 | %FileCheck -check-prefix=CHECK-ONONE %s
// RUN: %target-swiftc_driver -### -g -O %s 2>&1 | %FileCheck -check-prefix=CHECK-O %s
// RUN: %target-swiftc_driver -### -g -Osize %s 2>&1 | %FileCheck -check-prefix=CHECK-OSIZE %s

// CHECK: -enable-anonymous-context-mangled-names
// CHECK-ONONE: -enable-anonymous-context-mangled-names
// CHECK-O-NOT: -enable-anonymous-context-mangled-names
// CHECK-OSIZE-NOT: -enable-anonymous-context-mangled-names

