/// Test diagnostics with module aliasing.
///
/// Module 'Lib' imports module 'XLogging', and 'XLogging' is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create AppleLogging.swiftmodule by aliasing XLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule

/// 1. Pass: load and reference a module with module aliasing
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name LibA %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibA.swiftmodule -Rmodule-loading 2> %t/result-LibA.output
// RUN: test -f %t/LibA.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-LibA.output -check-prefix CHECK-LOAD
// CHECK-LOAD: remark: loaded module {{.*}}AppleLogging.swiftmodule

/// 2. Fail: trying to access a non-member of a module (with module aliasing) should fail with the module alias in the diags
/// Try building module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibB %t/FileLibNoSuchMember.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibB.swiftmodule 2> %t/result-LibB.output
// RUN: %FileCheck %s -input-file %t/result-LibB.output -check-prefix CHECK-NO-MEMBER
// CHECK-NO-MEMBER: error: module 'XLogging' has no member named 'setupErr'

/// 3. Fail: importing the real module name that's being aliased should fail
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibC %t/FileLibImportRealName.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibC.swiftmodule 2> %t/result-LibC.output
// RUN: %FileCheck %s -input-file %t/result-LibC.output -check-prefix CHECK-NOT-IMPORT
// CHECK-NOT-IMPORT: error: cannot refer to module as 'AppleLogging' because it has been aliased; use 'XLogging' instead

/// 4-1. Fail: referencing the real module name that's aliased should fail
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibD %t/FileLibRefRealName1.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibD.swiftmodule 2> %t/result-LibD.output
// RUN: %FileCheck %s -input-file %t/result-LibD.output -check-prefix CHECK-NOT-REF1
// CHECK-NOT-REF1: error: cannot find 'AppleLogging' in scope

/// 4-2. Fail: referencing the real module name that's aliased should fail
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibE %t/FileLibRefRealName2.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibE.swiftmodule 2> %t/result-LibE.output
// RUN: %FileCheck %s -input-file %t/result-LibE.output -check-prefix CHECK-NOT-REF2
// CHECK-NOT-REF2: error: cannot find type 'AppleLogging' in scope

/// 4-3. Fail: referencing the real module name that's aliased should fail
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibF %t/FileLibRefRealName3.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibF.swiftmodule 2> %t/result-LibF.output
// RUN: %FileCheck %s -input-file %t/result-LibF.output -check-prefix CHECK-NOT-REF3
// CHECK-NOT-REF3: error: cannot find type 'AppleLogging' in scope

/// 4-4. Fail: referencing the real module name that's aliased should fail
/// Create module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: not %target-swift-frontend -module-name LibG %t/FileLibRefRealName4.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/LibG.swiftmodule 2> %t/result-LibG.output
// RUN: %FileCheck %s -input-file %t/result-LibG.output -check-prefix CHECK-NOT-REF4
// CHECK-NOT-REF4: error: cannot find type 'AppleLogging' in scope


// BEGIN FileLogging.swift
public protocol Loggable {
  var verbosity: Int { get }
}

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

// BEGIN FileLibNoSuchMember.swift
import XLogging

public func start() {
  _ = XLogging.setupErr()
}

// BEGIN FileLibImportRealName.swift
import XLogging
import AppleLogging

public func start() {
  _ = XLogging.setup()
}

// BEGIN FileLibRefRealName1.swift
import XLogging

public func start() {
  _ = AppleLogging.setup()
}

// BEGIN FileLibRefRealName2.swift
import XLogging

public struct MyStruct: AppleLogging.Loggable {
  public var verbosity: Int {
    return 3
  }
}

// BEGIN FileLibRefRealName3.swift
import XLogging

public struct MyStruct<T> where T: AppleLogging.Loggable {
  func log<T>(_ arg: T) {
  }
}

// BEGIN FileLibRefRealName4.swift
import XLogging

public struct MyStruct {
  func log<T: AppleLogging.Loggable>(_ arg: T) {
  }
}
