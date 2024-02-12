// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s 2>^1 | %FileCheck %s

// CHECK: -plugin-path
// CHECK-SAME: {{(/|\\\\)}}lib{{(/|\\\\)}}swift{{(/|\\\\)}}host{{(/|\\\\)}}plugins

// CHECK-SAME: -plugin-path
// CHECK-SAME: {{(/|\\\\)}}local{{(/|\\\\)}}lib{{(/|\\\\)}}swift{{(/|\\\\)}}host{{(/|\\\\)}}plugins
