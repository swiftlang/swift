/// Test a serialized module contains the module real names when -module-alias flag is passed.
///
/// Module 'ClientN' imports 'Lib', and 'Lib' imports module 'XLogging'.
/// 'XLogging' needs to be aliased due to a collision, so it's aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging via -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

/// Check AppleLogging.swiftmodule correctly contains AppleLogging as module name in the binary
// RUN: llvm-bcanalyzer --dump %t/AppleLogging.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-FOUND
// BCANALYZER-FOUND: MODULE_NAME{{.*}}AppleLogging

// RUN: not llvm-bcanalyzer --dump %t/AppleLogging.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-NOT-FOUND
// BCANALYZER-NOT-FOUND: MODULE_NAME{{.*}}XLogging

/// Create module Lib that imports XLogging with -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

/// Check AppleLogging.swiftmodule is loaded
// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-LOAD1
// CHECK-LOAD1: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// Check XLogging.swiftmodule is not loaded
// RUN: not %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-NOT-LOAD1
// CHECK-NOT-LOAD1: remark: loaded module at {{.*}}XLogging.swiftmodule

/// Check Lib.swiftmodule contains AppleLogging and not XLogging as an imported module
/// in the binary
// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-IMPORT1
// BCANALYZER-IMPORT1: IMPORTED_MODULE{{.*}}AppleLogging

// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-NOT-IMPORT1
// BCANALYZER-NOT-IMPORT1: IMPORTED_MODULE{{.*}}XLogging

/// Create a module Client that imports Lib, without module aliasing for XLogging
// RUN: %target-swift-frontend -module-name Client %t/FileClient.swift -I %t -emit-module -emit-module-path %t/Client.swiftmodule -Rmodule-loading 2> %t/result-Client.output

/// Check Client.swiftmodule is created and Lib.swiftmodule and AppleLogging.swiftmodule are loaded
// RUN: test -f %t/Client.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Client.output -check-prefix CHECK-LOAD2
// CHECK-LOAD2: remark: loaded module at {{.*}}Lib.swiftmodule

/// Check Client.swiftmodule contains Lib as an imported module in the binary
// RUN: llvm-bcanalyzer --dump %t/Client.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER-IMPORT2
// BCANALYZER-IMPORT2: IMPORTED_MODULE{{.*}}Lib


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

// BEGIN FileClient.swift
import XLogging
import Lib
public func rubLib() {
  Lib.start()
}
public func runLog() {
  _ = XLogging.setup()
}
