/// Test AST with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify AST contains AppleLogging as module name
// RUN: %target-swift-frontend -dump-ast %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t > %t/result-ast.output

// RUN: %FileCheck %s -input-file %t/result-ast.output -check-prefix CHECK-AST
// CHECK-AST-NOT: module<XLogging>
// CHECK-AST-NOT: decl="XLogging"
// CHECK-AST: module<AppleLogging>
// CHECK-AST: decl="AppleLogging"

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
