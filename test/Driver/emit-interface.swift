// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-parseable-module-interface -o %t/foo 2>&1 | %FileCheck %s

// CHECK: swift -frontend
// CHECK-SAME: emit-interface.swift
// CHECK: swift -frontend -merge-modules
// CHECK-SAME: -emit-parseable-module-interface-path {{.+}}/foo.swiftinterface
// CHECK: bin/ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-parseable-module-interface -o %t/foo -force-single-frontend-invocation 2>&1 | %FileCheck -check-prefix=CHECK-WHOLE-MODULE %s

// CHECK-WHOLE-MODULE: swift -frontend
// CHECK-WHOLE-MODULE-SAME: emit-interface.swift
// CHECK-WHOLE-MODULE-SAME: -emit-parseable-module-interface-path {{.+}}/foo.swiftinterface
// CHECK-WHOLE-MODULE-NOT: -merge-modules
// CHECK-WHOLE-MODULE: bin/ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-parseable-module-interface-path %t/unrelated.swiftinterface -o %t/foo -force-single-frontend-invocation 2>&1 | %FileCheck -check-prefix=CHECK-EXPLICIT-PATH %s

// CHECK-EXPLICIT-PATH: swift -frontend
// CHECK-EXPLICIT-PATH-SAME: emit-interface.swift
// CHECK-EXPLICIT-PATH-SAME: -emit-parseable-module-interface-path {{.+}}/unrelated.swiftinterface

// Ensure that we emit arguments when we force filelists as well
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-parseable-module-interface -o %t/foo -module-name foo -force-single-frontend-invocation -driver-filelist-threshold=0 2>&1 | %FileCheck -check-prefix=CHECK-FILELIST %s

// CHECK-FILELIST: swift -frontend
// CHECK-FILELIST-SAME: -supplementary-output-file-map
// CHECK-FILELIST-NOT: emit-interface.swift{{ }}


// Actually run this and make sure the flags show up in the interface

// RUN: %swiftc_driver -target x86_64-apple-macosx10.9 %s -emit-parseable-module-interface -o %t/foo -module-name foo -force-single-frontend-invocation -driver-filelist-threshold=0 2>&1
// RUN: %FileCheck %s < %t/foo.swiftinterface --check-prefix CHECK-FILELIST-INTERFACE

// CHECK-FILELIST-INTERFACE: swift-module-flags:
// CHECK-FILELIST-INTERFACE-SAME: -target x86_64-apple-macosx10.9
// CHECK-FILELIST-INTERFACE-SAME: -module-name foo

