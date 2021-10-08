/// Test the -module-alias flag with the following scenario:
/// Module 'Utils' imports module 'LoggingApple', and module 'UserN' imports both 'Utils' and 'Logging'.
/// 'Logging' can be aliased 'LoggingApple' via -module-alias.

// RUN: %empty-directory(%t)

/// Create Logging.swiftmodule
// RUN: echo 'public struct Logger { }' > %t/FileLog.swift
// RUN: echo 'public func setup() -> Logging.Logger? { return nil }' >> %t/FileLog.swift
// RUN: %target-swift-frontend -module-name Logging %t/FileLog.swift -emit-module -emit-module-path %t/Logging.swiftmodule

/// Check Logging.swiftmodule is created
// RUN: test -f %t/Logging.swiftmodule

/// Create LoggingApple.swiftmodule
// RUN: echo 'public struct Logger { }' > %t/FileAppleLog.swift
// RUN: echo 'public func setup() -> LoggingApple.Logger? { return nil }' >> %t/FileAppleLog.swift
// RUN: echo 'public func pie() -> LoggingApple.Logger? { return nil }' >> %t/FileAppleLog.swift
// RUN: %target-swift-frontend -module-name LoggingApple %t/FileAppleLog.swift -emit-module -emit-module-path %t/LoggingApple.swiftmodule

/// Check LoggingApple.swiftmodule is created
// RUN: test -f %t/LoggingApple.swiftmodule

/// Create a module Utils that imports LoggingApple
// RUN: echo 'import LoggingApple' > %t/FileUtils.swift
// RUN: echo 'public func start() { _ = LoggingApple.setup() }' >> %t/FileUtils.swift
// RUN: %target-swift-frontend -module-name Utils %t/FileUtils.swift -I %t -emit-module -emit-module-path %t/Utils.swiftmodule -Rmodule-loading 2> %t/result-Utils.output

/// Check Utils.swiftmodule is created and LoggingApple.swiftmodule is loaded
// RUN: test -f %t/Utils.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Utils.output -check-prefix CHECK-UTILS
// CHECK-UTILS: remark: loaded module at {{.*}}LoggingApple.swiftmodule

/// Create a module User1 that imports both Utils and Logging, without module aliasing for Logging
// RUN: echo 'import Logging' > %t/File.swift
// RUN: echo 'import Utils' >> %t/File.swift
// RUN: echo 'public func runUtils() { Utils.start() }' >> %t/File.swift
// RUN: echo 'public func runLog() { _ = Logging.setup() }' >> %t/File.swift
// RUN: %target-swift-frontend -module-name User1 %t/File.swift -I %t -emit-module -emit-module-path %t/User1.swiftmodule -Rmodule-loading 2> %t/result-User1.output

/// Check User1.swiftmodule is created and Utils.swiftmodule and Logging.swiftmodule are loaded
// RUN: test -f %t/User1.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-User1.output -check-prefix CHECK-1
// CHECK-1: remark: loaded module at {{.*}}Logging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}Utils.swiftmodule

/// Create a module User2 that imports both Utils and Logging, with module aliasing for Logging
// RUN: echo 'import Logging' > %t/File.swift
// RUN: echo 'import Utils' >> %t/File.swift
// RUN: echo 'public func runUtils() { Utils.start() }' >> %t/File.swift
// RUN: echo 'public func runLog() { _ = Logging.setup() }' >> %t/File.swift
// RUN: %target-swift-frontend -module-name User2 -module-alias Logging=LoggingApple %t/File.swift -I %t -emit-module -emit-module-path %t/User2.swiftmodule -Rmodule-loading 2> %t/result-User2.output

/// Check User2.swiftmodule is created and Utils.swiftmodule and LoggingApple.swiftmodule are loaded but
/// Logging.swiftmodule is not loaded.
// RUN: test -f %t/User2.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-User2.output -check-prefix CHECK-2A
// CHECK-2A: remark: loaded module at {{.*}}LoggingApple.swiftmodule
// CHECK-2A: remark: loaded module at {{.*}}Utils.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-User2.output -check-prefix CHECK-2B
// CHECK-2B: remark: loaded module at {{.*}}Logging.swiftmodule
