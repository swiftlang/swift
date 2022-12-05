/// Test a serialized interface contains the module real names when -module-alias flag is passed.
///
/// 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// 1. AppleLogging
/// Create AppleLogging.swiftmodule by aliasing XLogging via -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule -enable-library-evolution

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

/// Check AppleLogging.swiftmodule correctly contains AppleLogging as module name in the binary
// RUN: llvm-bcanalyzer --dump %t/AppleLogging.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-FOUND
// BCANALYZER-FOUND: MODULE_NAME{{.*}}AppleLogging

// RUN: llvm-bcanalyzer --dump %t/AppleLogging.swiftmodule | not grep XLogging

/// 2. Lib
/// Create an interface for Lib that imports XLogging with -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-interface-path %t/Lib.swiftinterface -swift-version 5 -enable-library-evolution -I %t -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftinterface is created
// RUN: test -f %t/Lib.swiftinterface
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

/// Check AppleLogging.swiftmodule is loaded
// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-LOAD
// CHECK-LOAD: remark: loaded module {{.*}}AppleLogging.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-NOT-LOAD1
// CHECK-NOT-LOAD1: remark: loaded module {{.*}}XLogging.swiftmodule

/// Check imported modules contain AppleLogging, not XLogging
// RUN: %FileCheck %s -input-file %t/Lib.swiftinterface -check-prefix CHECK-IMPORT
// CHECK-IMPORT: -module-alias XLogging=AppleLogging
// CHECK-IMPORT: import AppleLogging
// RUN: not %FileCheck %s -input-file %t/Lib.swiftinterface -check-prefix CHECK-NOT-IMPORT
// CHECK-NOT-IMPORT: import XLogging


// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib.swift
import XLogging

public func start() {
  _ = XLogging.setup()
}
