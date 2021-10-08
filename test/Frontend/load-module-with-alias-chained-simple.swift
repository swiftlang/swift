/// Test the -module-alias flag on a sample scenario:
/// Module 'Utils' imports module 'Logging'.
/// 'Logging' needs to be aliased due to a name collision, so is renamed 'AppleLogging'.

// RUN: %empty-directory(%t)

/// Logging has a source file that has a reference to its module name Logging
// RUN: echo 'public struct Logger { }' > %t/FileLog.swift
// RUN: echo 'public func setup() -> Logging.Logger? { return nil }' >> %t/FileLog.swift

/// Create AppleLogging.swiftmodule by aliasing Logging via -module-alias Logging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias Logging=AppleLogging %t/FileLog.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

/// Create a module Utils that imports Logging with -module-alias Logging=AppleLogging
// RUN: echo 'import Logging' > %t/FileUtils.swift
// RUN: echo 'public func start() { Logging.setup() }' >> %t/FileUtils.swift
// RUN: %target-swift-frontend -module-name Utils %t/FileUtils.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/Utils.swiftmodule -Rmodule-loading 2> %t/load-result.output

/// Check Utils.swiftmodule is created and AppleLogging.swiftmodule is loaded
// RUN: test -f %t/Utils.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

// RUN: %FileCheck %s -input-file %t/load-result.output -check-prefix CHECK-LOAD
// CHECK-LOAD: remark: loaded module at {{.*}}AppleLogging.swiftmodule

