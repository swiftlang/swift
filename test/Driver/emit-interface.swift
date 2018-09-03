// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -experimental-emit-interface -o %t/foo 2>&1 | %FileCheck %s

// CHECK: swift -frontend
// CHECK-SAME: emit-interface.swift
// CHECK: swift -frontend -merge-modules
// CHECK-SAME: -emit-interface-path {{.+}}/foo.swiftinterface
// CHECK: bin/ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -experimental-emit-interface -o %t/foo -force-single-frontend-invocation 2>&1 | %FileCheck -check-prefix=CHECK-WHOLE-MODULE %s

// CHECK-WHOLE-MODULE: swift -frontend
// CHECK-WHOLE-MODULE-SAME: emit-interface.swift
// CHECK-WHOLE-MODULE-SAME: -emit-interface-path {{.+}}/foo.swiftinterface
// CHECK-WHOLE-MODULE-NOT: -merge-modules
// CHECK-WHOLE-MODULE: bin/ld
