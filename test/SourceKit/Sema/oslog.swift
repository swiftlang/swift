// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-build-swift -emit-module -module-name Lib -o %t -Xfrontend -experimental-allow-module-with-compiler-errors -Xfrontend -experimental-skip-all-function-bodies %t/lib.swift
// RUN: %sourcekitd-test -req=sema %t/main.swift -- %t/main.swift -I%t -sdk %sdk -Xfrontend -experimental-allow-module-with-compiler-errors | %FileCheck %s
// CHECK-NOT: oslog_invalid_log_message

// BEGIN lib.swift
import os

public struct Foo {
  public let prop: String
  public init() { self.prop = "boop" }
}

@available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension OSLogInterpolation {
  @_optimize(none)
  @_transparent
  @_semantics("oslog.requires_constant_arguments")
  public mutating func appendInterpolation(_ value: @autoclosure @escaping () -> Foo) {
    let v = value()
    appendInterpolation(v.prop)
  }
}

// BEGIN main.swift
import os
import Lib

if #available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
  let logger = Logger()
  logger.log("Log a foo: \(Foo())")
}
