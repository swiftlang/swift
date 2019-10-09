// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/build/Private
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file | %FileCheck %s -check-prefix CHECK-PRIVATE

// CHECK-PRIVATE: build{{/|\\\\}}Private{{/|\\\\}}sourceinfo_file.swiftsourceinfo

// RUN: %empty-directory(%t/build)
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file | %FileCheck %s -check-prefix CHECK-NOPRIVATE

// CHECK-NOPRIVATE-NOT: Private/sourceinfo_file.swiftsourceinfo
// CHECK-NOPRIVATE: build{{/|\\\\}}sourceinfo_file.swiftsourceinfo

// RUN: %empty-directory(%t/build)
// RUN: %swiftc_driver -driver-print-jobs -emit-module %s -emit-module-path %t/build/sourceinfo_file.swiftmodule -module-name sourceinfo_file -emit-module-source-info-path %t/build/DriverPath.swiftsourceinfo | %FileCheck %s -check-prefix CHECK-DRIVER-OPT

// CHECK-DRIVER-OPT: build{{[/\\]}}DriverPath.swiftsourceinfo
