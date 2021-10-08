/// Test the -module-alias flag on the following scenario:
/// Module 'Utils' imports module 'Logging', and module 'UserN' imports both 'Utils' and 'Logging'.
/// 'Logging' needs to be aliased due to a name collision, so is renamed 'AppleLogging'.

// RUN: %empty-directory(%t)

/// Input file with a reference to its enclosing module called Logging
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
// RUN: %target-swift-frontend -module-name Utils %t/FileUtils.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/Utils.swiftmodule -Rmodule-loading 2> %t/result-Utils.output

/// Check Utils.swiftmodule is created and AppleLogging.swiftmodule is loaded
// RUN: test -f %t/Utils.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Utils.output -check-prefix CHECK-UTILS
// CHECK-UTILS: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// Create a module User1 that imports both Utils and Logging, with module aliasing for Logging
// RUN: echo 'import Logging' > %t/File.swift
// RUN: echo 'import Utils' >> %t/File.swift
// RUN: echo 'public func run() { Utils.start() }' >> %t/File.swift
// RUN: %target-swift-frontend -module-name User1 %t/File.swift -module-alias Logging=AppleLogging -I %t -emit-module -emit-module-path %t/User1.swiftmodule -Rmodule-loading 2> %t/result-User1.output

/// Check User1.swiftmodule is created and Utils.swiftmodule and AppleLogging.swiftmodule are loaded
// RUN: test -f %t/User1.swiftmodule
// RUN: test -f %t/Utils.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/Logging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-User1.output -check-prefix CHECK-1
// CHECK-1: remark: loaded module at {{.*}}AppleLogging.swiftmodule
// CHECK-1: remark: loaded module at {{.*}}Utils.swiftmodule

/// Try creating a module User2 that imports both Utils and Logging, without module aliasing
// RUN: not %target-swift-frontend -module-name User2 %t/File.swift -I %t -emit-module -emit-module-path %t/User2.swiftmodule 2> %t/result-User2.output

/// Check that it fails
// RUN: %FileCheck %s -input-file %t/result-User2.output -check-prefix CHECK-2
// CHECK-2: {{.*}}error: no such module 'Logging'

/// Try creating a module User3 that imports both Utils and AppleLogging, without module aliasing
// RUN: echo 'import AppleLogging' > %t/AnotherFile.swift
// RUN: echo 'import Utils' >> %t/AnotherFile.swift
// RUN: echo 'public func run() { _ = AppleLogging.setup() }' >> %t/AnotherFile.swift
// RUN: not %target-swift-frontend -module-name User3 %t/AnotherFile.swift -I %t -emit-module -emit-module-path %t/User3.swiftmodule 2> %t/result-User3.output

/// Check that it fails
// RUN: %FileCheck %s -input-file %t/result-User3.output -check-prefix CHECK-3
// CHECK-3: {{.*}}error: missing required module 'Logging'

/// Try creating a module User4 that imports both Utils and AppleLogging, with module aliasing for Logging
// RUN: %target-swift-frontend -module-name User4 -module-alias Logging=AppleLogging %t/AnotherFile.swift -I %t -emit-module -emit-module-path %t/User4.swiftmodule -Rmodule-loading 2> %t/result-User4.output

/// Check that it fails
// RUN: %FileCheck %s -input-file %t/result-User4.output -check-prefix CHECK-4
// CHECK-4: remark: loaded module at {{.*}}AppleLogging.swiftmodule
// CHECK-4: remark: loaded module at {{.*}}Utils.swiftmodule
