/// Test the -module-alias flag on the following scenario:
/// Module 'ClientN' imports 'XLogging' and 'Lib', and 'Lib' imports 'XLogging'.
/// 'XLogging' needs to be aliased due to a name collision, so is aliased 'AppleLogging'.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// 1. AppleLogging
/// Create AppleLogging.swiftmodule by aliasing XLogging via -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name AppleLogging -module-alias XLogging=AppleLogging %t/FileLogging.swift -emit-module -emit-module-path %t/AppleLogging.swiftmodule

/// Check AppleLogging.swiftmodule is created
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

/// 2. Lib
/// Create a module Lib that imports XLogging WITH -module-alias XLogging=AppleLogging
// RUN: %target-swift-frontend -module-name Lib %t/FileLib.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Lib.swiftmodule -Rmodule-loading 2> %t/result-Lib.output

/// Check Lib.swiftmodule is created and AppleLogging.swiftmodule is loaded
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-Lib
// CHECK-Lib: remark: loaded module {{.*}}AppleLogging.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Lib.output -check-prefix CHECK-NOT-Lib
// CHECK-NOT-Lib: remark: loaded module {{.*}}XLogging.swiftmodule

/// 3a. Client1
/// Create module Client1 that imports Lib and XLogging, WITH module aliasing for XLogging
// RUN: %target-swift-frontend -module-name Client1 %t/FileClient.swift -module-alias XLogging=AppleLogging -I %t -emit-module -emit-module-path %t/Client1.swiftmodule -Rmodule-loading -Rmodule-loading 2> %t/result-Client1.output

/// Check Client1.swiftmodule is created and Lib.swiftmodule and AppleLogging.swiftmodule are loaded
// RUN: test -f %t/Client1.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule
// RUN: %FileCheck %s -input-file %t/result-Client1.output -check-prefix CHECK-CLIENT1
// CHECK-CLIENT1: remark: loaded module {{.*}}AppleLogging.swiftmodule
// CHECK-CLIENT1: remark: loaded module {{.*}}Lib.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Client1.output -check-prefix CHECK-NOT-CLIENT1
// CHECK-NOT-CLIENT1: remark: loaded module {{.*}}XLogging.swiftmodule

/// 3b. Client2
/// Try creating module Client2 that imports Lib and XLogging, WITHOUT module aliasing
// RUN: not %target-swift-frontend -module-name Client2 %t/FileClient.swift -I %t -emit-module -emit-module-path %t/Client2.swiftmodule 2> %t/result-Client2.output

/// Check that it fails
// RUN: %FileCheck %s -input-file %t/result-Client2.output -check-prefix CHECK-CLIENT2
// CHECK-CLIENT2: {{.*}}error: no such module 'XLogging'

/// 3c. Client3
/// Create module Client3 that imports Lib and AppleLogging, WITHOUT module aliasing
// RUN: %target-swift-frontend -module-name Client3 %t/FileClientOther.swift -I %t -emit-module -emit-module-path %t/Client3.swiftmodule -Rmodule-loading 2> %t/result-Client3.output

/// Check Client3.swiftmodule is created and correct modules are loaded
// RUN: test -f %t/Client3.swiftmodule
// RUN: test -f %t/Lib.swiftmodule
// RUN: test -f %t/AppleLogging.swiftmodule
// RUN: not test -f %t/XLogging.swiftmodule

// RUN: %FileCheck %s -input-file %t/result-Client3.output -check-prefix CHECK-CLIENT3
// CHECK-CLIENT3: remark: loaded module {{.*}}AppleLogging.swiftmodule
// CHECK-CLIENT3: remark: loaded module {{.*}}Lib.swiftmodule
// RUN: not %FileCheck %s -input-file %t/result-Client3.output -check-prefix CHECK-NOT-CLIENT3
// CHECK-NOT-CLIENT3: remark: loaded module {{.*}}XLogging.swiftmodule

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

// BEGIN FileClient.swift
import XLogging
import Lib
public func rubLib() {
  Lib.start()
}
public func runLog() {
  _ = XLogging.setup()
}


// BEGIN FileClientOther.swift
import AppleLogging
import Lib
public func rubLib() {
  Lib.start()
}
public func runLog() {
  _ = AppleLogging.setup()
}

