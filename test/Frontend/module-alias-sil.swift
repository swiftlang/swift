/// Test SIL with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify generated SIL contains AppleLogging, and XLogging only appears in import lines in the prologue
// RUN: %target-swift-frontend -emit-sil %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t > %t/result-sil.output
// RUN: %FileCheck %s -input-file %t/result-sil.output -check-prefix CHECK-SIL
// CHECK-SIL: import XLogging
// CHECK-SIL: s12AppleLogging
// RUN: not %FileCheck %s -input-file %t/result-sil.output -check-prefix CHECK-NOT-SIL
// CHECK-NOT-SIL: s8XLogging

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib.swift
import XLogging

public func start() -> XLogging.Logger? {
  return XLogging.setup()
}

public func end(_ arg: XLogging.Logger) {
}
