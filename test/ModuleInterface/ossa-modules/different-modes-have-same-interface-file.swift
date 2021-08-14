// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/MCP)

// This test validates that emit-ossa-modules does not effect the swift
// interface file emitted.

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleDefault.swiftinterface.enable_ossa -parse-stdlib %s -module-name SwiftModuleDefault -enable-library-evolution -swift-version 5 -enable-ossa-modules
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleDefault.swiftinterface -parse-stdlib %s -module-name SwiftModuleDefault -enable-library-evolution -swift-version 5
// RUN: diff -u %t/SwiftModuleDefault.swiftinterface.enable_ossa %t/SwiftModuleDefault.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOnone.swiftinterface.enable_ossa -parse-stdlib %s -module-name SwiftModuleOnone -enable-library-evolution -swift-version 5 -enable-ossa-modules -Onone
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOnone.swiftinterface -parse-stdlib %s -module-name SwiftModuleOnone -enable-library-evolution -swift-version 5 -Onone
// RUN: diff -u %t/SwiftModuleOnone.swiftinterface.enable_ossa %t/SwiftModuleOnone.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleO.swiftinterface.enable_ossa -parse-stdlib %s -module-name SwiftModuleO -enable-library-evolution -swift-version 5 -enable-ossa-modules -O
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleO.swiftinterface -parse-stdlib %s -module-name SwiftModuleO -enable-library-evolution -swift-version 5 -O
// RUN: diff -u %t/SwiftModuleO.swiftinterface.enable_ossa %t/SwiftModuleO.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOsize.swiftinterface.enable_ossa -parse-stdlib %s -module-name SwiftModuleOsize -enable-library-evolution -swift-version 5 -enable-ossa-modules -Osize
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOsize.swiftinterface -parse-stdlib %s -module-name SwiftModuleOsize -enable-library-evolution -swift-version 5 -Osize
// RUN: diff -u %t/SwiftModuleOsize.swiftinterface.enable_ossa %t/SwiftModuleOsize.swiftinterface

@_fixed_layout
public final class Klass {
  public init() {
  }
}

@inlinable
public func foo() -> Klass {
  var a = Klass()
  return a
}
