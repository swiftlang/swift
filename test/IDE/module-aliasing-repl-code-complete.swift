/// Test REPL code completion with module aliasing
/// When -module-alias <alias_name>=<real_name> is applied, code completion should show
/// the <alias_name> as that's the name which should appear in source files including import statements,
/// decls, expressions, etc. while getting visible decls come from the module of <real_name>, which
/// is the name of the binary.
/// Below, XLogging is the alias and mapped to the real name AppleLogging. Note that the binary name
/// AppleLogging should not appear in the code completion results.
///
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/FileLogging.swift -module-name AppleLogging -module-alias XLogging=AppleLogging -emit-module -o %t/AppleLogging.swiftmodule

// RUN: %target-swift-ide-test -repl-code-completion -source-filename %t/FileLib1.swift -module-alias XLogging=AppleLogging -I %t > %t/result1.txt
// RUN: %FileCheck %s < %t/result1.txt

// RUN: %target-swift-ide-test -repl-code-completion -source-filename %t/FileLib2.swift -module-alias XLogging=AppleLogging -I %t > %t/result2.txt
// RUN: %FileCheck %s < %t/result2.txt

// CHECK-NOT: AppleLogging
// CHECK: XLogging

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}

public protocol Logging {
  var content: String { get }
}

public func setupLogger() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib1.swift
import

// BEGIN FileLib2.swift
import XLogging
func f() {
  X
}

