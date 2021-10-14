/// Test the -module-alias flag with the following scenario:
/// Module 'ClientN' imports 'XLogging' and 'Lib', and 'Lib' imports module 'AppleLogging'.
/// 'XLogging' can be aliased 'AppleLogging' via -module-alias.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// 1a. XLogging
/// Create XLogging.swiftmodule
// RUN: %target-swift-frontend -module-name XLogging %t/FileLogging.swift -emit-module -emit-module-path %t/XLogging.swiftmodule
// RUN: test -f %t/XLogging.swiftmodule

/// 1b. AppleLogging
/// Create AppleLogging.swiftmodule
// RUN: %target-swift-frontend -module-name AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// 2. Lib
/// Create module Lib that imports AppleLogging
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created and AppleLogging.swiftmodule is loaded
// RUN: test -f %t/Lib.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-Lib
// CHECK-Lib: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// 3a. Client1
/// Create module Client1 that imports Lib and XLogging, WITHOUT module aliasing for XLogging
// RUN: %target-swift-frontend -module-name Client1 %t/FileClient.swift -I %t -emit-module -emit-module-path %t/Client1.swiftmodule -Rmodule-loading 2> %t/result-Client1.output

/// Check Client1.swiftmodule is created and Lib.swiftmodule and XLogging.swiftmodule are loaded
// RUN: test -f %t/Client1.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Client1.output -check-prefix CHECK-1
// CHECK-1: remark: loaded module at {{.*}}XLogging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}Lib.swiftmodule

/// 3b. Client2
/// Create a module Client2 that imports Lib and XLogging, WITH module aliasing for XLogging
// RUN: %target-swift-frontend -module-name Client2 -module-alias XLogging=AppleLogging %t/FileClient.swift -I %t -emit-module -emit-module-path %t/Client2.swiftmodule -Rmodule-loading 2> %t/result-Client2.output

/// Check Client2.swiftmodule is created and Lib.swiftmodule and AppleLogging.swiftmodule are
/// loaded but XLogging.swiftmodule is not loaded.
// RUN: test -f %t/Client2.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-2A
// CHECK-2A: remark: loaded module at {{.*}}AppleLogging.swiftmodule
// CHECK-2A: remark: loaded module at {{.*}}Lib.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-2B
// CHECK-2B: remark: loaded module at {{.*}}XLogging.swiftmodule


// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}
public func setup() -> Logger? {
  return Logger()
}

// BEGIN FileLib.swift
import AppleLogging

public func start() {
  _ = AppleLogging.setup()
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
