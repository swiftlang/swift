// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-module-interface -o %t/foo 2>&1 | %FileCheck %s

// CHECK: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-SAME: emit-interface.swift
// CHECK: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend -merge-modules
// CHECK-SAME: -emit-module-interface-path {{.+[/\\]}}foo.swiftinterface
// CHECK: {{(bin/)?}}ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-module-interface -o %t/foo -whole-module-optimization 2>&1 | %FileCheck -check-prefix=CHECK-WHOLE-MODULE %s

// CHECK-WHOLE-MODULE: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-WHOLE-MODULE-SAME: emit-interface.swift
// CHECK-WHOLE-MODULE-SAME: -emit-module-interface-path {{.+[/\\]}}foo.swiftinterface
// CHECK-WHOLE-MODULE-NOT: -merge-modules
// CHECK-WHOLE-MODULE: {{(bin/)?}}ld

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-module-interface-path %t/unrelated.swiftinterface -o %t/foo -whole-module-optimization 2>&1 | %FileCheck -check-prefix=CHECK-EXPLICIT-PATH %s

// CHECK-EXPLICIT-PATH: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-EXPLICIT-PATH-SAME: emit-interface.swift
// CHECK-EXPLICIT-PATH-SAME: -emit-module-interface-path {{.+[/\\]}}unrelated.swiftinterface

// Ensure that we emit arguments when we force filelists as well
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s -emit-module-interface -o %t/foo -module-name foo -whole-module-optimization -driver-filelist-threshold=0 2>&1 | %FileCheck -check-prefix=CHECK-FILELIST %s

// CHECK-FILELIST: swift{{(c|c-legacy-driver|-frontend)?(\.exe)?"?}} -frontend
// CHECK-FILELIST-SAME: -supplementary-output-file-map
// CHECK-FILELIST-NOT: emit-interface.swift{{ }}
