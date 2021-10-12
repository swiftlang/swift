/// Test the -module-alias flag with the following scenario:
/// Module 'Lib' imports module 'LoggingApple', and module 'ClientN' imports both 'Lib' and 'Logging'.
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

/// Create a module Lib that imports LoggingApple
// RUN: echo 'import LoggingApple' > %t/FileLib.swift
// RUN: echo 'public func start() { _ = LoggingApple.setup() }' >> %t/FileLib.swift
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created and LoggingApple.swiftmodule is loaded
// RUN: test -f %t/Lib.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-Lib
// CHECK-Lib: remark: loaded module at {{.*}}LoggingApple.swiftmodule

/// Create a module Client1 that imports both Lib and Logging, without module aliasing for Logging
// RUN: echo 'import Logging' > %t/File.swift
// RUN: echo 'import Lib' >> %t/File.swift
// RUN: echo 'public func runLib() { Lib.start() }' >> %t/File.swift
// RUN: echo 'public func runLog() { _ = Logging.setup() }' >> %t/File.swift
// RUN: %target-swift-frontend -module-name Client1 %t/File.swift -I %t -emit-module -emit-module-path %t/Client1.swiftmodule -Rmodule-loading 2> %t/result-Client1.output

/// Check Client1.swiftmodule is created and Lib.swiftmodule and Logging.swiftmodule are loaded
// RUN: test -f %t/Client1.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Client1.output -check-prefix CHECK-1
// CHECK-1: remark: loaded module at {{.*}}Logging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}Lib.swiftmodule

/// Create a module Client2 that imports both Lib and Logging, with module aliasing for Logging
// RUN: echo 'import Logging' > %t/File.swift
// RUN: echo 'import Lib' >> %t/File.swift
// RUN: echo 'public func runLib() { Lib.start() }' >> %t/File.swift
// RUN: echo 'public func runLog() { _ = Logging.setup() }' >> %t/File.swift
// RUN: %target-swift-frontend -module-name Client2 -module-alias Logging=LoggingApple %t/File.swift -I %t -emit-module -emit-module-path %t/Client2.swiftmodule -Rmodule-loading 2> %t/result-Client2.output

/// Check Client2.swiftmodule is created and Lib.swiftmodule and LoggingApple.swiftmodule are loaded but
/// Logging.swiftmodule is not loaded.
// RUN: test -f %t/Client2.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-2A
// CHECK-2A: remark: loaded module at {{.*}}LoggingApple.swiftmodule
// CHECK-2A: remark: loaded module at {{.*}}Lib.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-2B
// CHECK-2B: remark: loaded module at {{.*}}Logging.swiftmodule
