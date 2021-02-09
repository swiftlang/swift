// RUN: %swiftc_driver -update-code -migrate-keep-objc-visibility -### %s 2>&1 | %FileCheck -check-prefix=CHECK-KEEP-OBJC %s

// CHECK-KEEP-OBJC-DAG: -migrate-keep-objc-visibility
// CHECK-KEEP-OBJC-DAG: -emit-remap-file-path {{.*}}.remap
