/// Test -emit-imported-modules with module aliasing.
///

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// Verify emitted imported modules contains AppleLogging as a module name
// RUN: %target-swift-frontend -emit-imported-modules %t/FileLib1.swift -module-alias XLogging=AppleLogging -I %t > %t/result1.output

// RUN: %FileCheck %s -input-file %t/result1.output -check-prefix CHECK-AST1
// CHECK-AST1-NOT: XLogging
// CHECK-AST1: AppleLogging

// RUN: %target-swift-frontend -emit-imported-modules %t/FileLib2.swift -module-alias XLogging=AppleLogging -I %t > %t/result2.output

// RUN: %FileCheck %s -input-file %t/result2.output -check-prefix CHECK-AST2
// CHECK-AST2-NOT: XLogging
// CHECK-AST2: AppleLogging


// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib1.swift
import XLogging

public func start() -> XLogging.Logger? {
  return XLogging.setup()
}

public func end(_ arg: XLogging.Logger) {
}

// BEGIN FileLib2.swift
import struct XLogging.Logger

public func start() -> XLogging.Logger? {
  return XLogging.Logger()
}
