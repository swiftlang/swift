// RUN: %swiftc_driver_plain -emit-executable %s -o %t.out -emit-module -emit-module-path %t.swiftmodule -emit-objc-header-path %t.h -serialize-diagnostics -emit-dependencies -parseable-output -driver-skip-execution 2>&1 | %FileCheck %s --check-prefix=CHECK-OFF
// RUN: %swiftc_driver_plain -emit-executable %s -o %t.out -emit-module -emit-module-path %t.swiftmodule -emit-objc-header-path %t.h -serialize-diagnostics -emit-dependencies -parseable-output -track-system-dependencies -driver-skip-execution 2>&1 | %FileCheck %s --check-prefix=CHECK-ON

// CHECK-OFF-NOT: -track-system-dependencies
// CHECK-ON: -track-system-dependencies
