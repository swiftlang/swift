/// Test -emit-imported-modules with module aliasing.
///

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify emitted imported modules contains AppleLogging as a module name
// RUN: %target-swift-frontend -emit-imported-modules %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t > %t/result.output

// RUN: %FileCheck %s -input-file %t/result.output -check-prefix CHECK-AST
// CHECK-AST: AppleLogging
// RUN: not %FileCheck %s -input-file %t/result.output -check-prefix CHECK-NOT-AST
// CHECK-NOT-AST: XLogging


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

