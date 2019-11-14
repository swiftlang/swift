// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/build/Project
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file | %FileCheck %s -check-prefix CHECK-PROJECT

// CHECK-PROJECT: build{{/|\\\\}}Project{{/|\\\\}}sourceinfo_file.swiftsourceinfo

// RUN: %empty-directory(%t/build)
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file | %FileCheck %s -check-prefix CHECK-NOPROJECT

// CHECK-NOPROJECT-NOT: Project/sourceinfo_file.swiftsourceinfo
// CHECK-NOPROJECT: build{{/|\\\\}}sourceinfo_file.swiftsourceinfo

// RUN: %empty-directory(%t/build)
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file -emit-module-source-info-path %t/build/DriverPath.swiftsourceinfo | %FileCheck %s -check-prefix CHECK-DRIVER-OPT

// CHECK-DRIVER-OPT: build{{[/\\]}}DriverPath.swiftsourceinfo
