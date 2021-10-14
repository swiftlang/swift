/// Test diagnostics with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// 1. No error
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name LibA %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibA.swiftmodule -Rmodule-loading 2> %t/result-LibA.output
// RUN: test -f %t/LibA.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-LibA.output -check-prefix CHECK-LOAD-A
// CHECK-LOAD-A: remark: loaded module at {{.*}}AppleLogging.swiftmodule

/// 2. Fail: No member error with module name 'XLogging'
/// Try building module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibB %t/FileLibNoMember.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibB.swiftmodule 2> %t/result-LibB.output
// RUN: %FileCheck %s -input-file %t/result-LibB.output -check-prefix CHECK-NO-MEMBER
// CHECK-NO-MEMBER: error: module 'XLogging' has no member named 'setupErr'

/// 3. FIXME: Should fail with module not found <rdar://83592084> -- Pass for now
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name LibC %t/FileLibRefRealName.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibC.swiftmodule 2> %t/result-LibC.output
// FIXME: toggle this to fail
// RUN: not %FileCheck %s -input-file %t/result-LibC.output -check-prefix CHECK-NOT-FOUND
// CHECK-NOT-FOUND: error: cannot find 'AppleLogging' in scope

/// 4. FIXME: Should fail with no such module error <rdar://83592084> -- Pass for now
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name LibC %t/FileLibImportRealName.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibC.swiftmodule 2> %t/result-LibC.output
// FIXME: toggle this to fail
// RUN: not %FileCheck %s -input-file %t/result-LibC.output -check-prefix CHECK-NOT-FOUND
// CHECK-NOT-FOUND: error: no such module 'AppleLogging'

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}
public func setup() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib.swift
import XLogging

public func start() {
  _ = XLogging.setup()
}

// BEGIN FileLibNoMember.swift
import XLogging

public func start() {
  _ = XLogging.setupErr()
}

// BEGIN FileLibRefRealName.swift
import XLogging

public func start() {
  _ = AppleLogging.setup()
}

// BEGIN FileLibImportRealName.swift
import XLogging
import AppleLogging

public func start() {
  _ = XLogging.setup()
}

