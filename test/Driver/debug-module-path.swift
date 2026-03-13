// RUN: %swiftc_driver -module-name a -driver-print-jobs -target x86_64-apple-macosx10.10 -g -o debugmodule-path %s 2>&1 | %FileCheck %s

// CHECK: -debug-module-path {{.*}}{{/|\\\\}}a.swiftmodule
