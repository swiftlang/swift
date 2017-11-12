// RUN: %swiftc_driver -update-code -migrate-keep-objc-visibility -### %s 2>&1 | %FileCheck -check-prefix=CHECK-KEEP-OBJC %s

// CHECK-KEEP-OBJC: -migrate-keep-objc-visibility
// CHECK-KEEP-OBJC: -emit-remap-file-path {{.*}}.remap
