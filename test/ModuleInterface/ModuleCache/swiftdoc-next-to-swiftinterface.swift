// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/Lib.swiftinterface -emit-module-doc -parse-stdlib -o %t/Lib.swiftmodule %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Lib -access-filter-public -I %t -source-filename=x -prefer-type-repr=false -fully-qualified-types=true > %t/from-module.txt
// RUN: %FileCheck %s < %t/from-module.txt

// RUN: rm %t/Lib.swiftmodule
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized %target-swift-ide-test -print-module -module-to-print=Lib -access-filter-public -I %t -source-filename=x -prefer-type-repr=false -fully-qualified-types=true > %t/from-interface.txt
// RUN: diff %t/from-module.txt %t/from-interface.txt

// Try again with architecture-specific subdirectories.
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Lib.swiftmodule)
// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/Lib.swiftmodule/%target-cpu.swiftinterface -emit-module-doc -parse-stdlib -o %t/Lib.swiftmodule/%target-swiftmodule-name -module-name Lib %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Lib -access-filter-public -I %t -source-filename=x -prefer-type-repr=false -fully-qualified-types=true > %t/from-module.txt
// RUN: %FileCheck %s < %t/from-module.txt

// RUN: rm %t/Lib.swiftmodule/%target-swiftmodule-name
// RUN: env SWIFT_FORCE_MODULE_LOADING=prefer-serialized %target-swift-ide-test -print-module -module-to-print=Lib -access-filter-public -I %t -source-filename=x -prefer-type-repr=false -fully-qualified-types=true > %t/from-interface.txt
// RUN: diff %t/from-module.txt %t/from-interface.txt

/// Very important documentation!
public struct SomeStructWithDocumentation {}

// CHECK: Very important documentation!
// CHECK-NEXT: struct SomeStructWithDocumentation {
