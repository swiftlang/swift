// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -g %s | %FileCheck %s

// CHECK: bin/swift -frontend{{.*}}-emit-module-path [[MOD:.*\.swiftmodule]]
// CHECK: bin/swift {{.*}}-emit-module [[MOD]]
// CHECK-SAME:                                 -o [[MERGED:.*\.swiftmodule]]
// CHECK: bin/swift -modulewrap [[MERGED]] -target x86_64-unknown-linux-gnu -o [[OBJ:.*\.o]]
// CHECK: bin/clang++{{.*}} [[OBJ]]
