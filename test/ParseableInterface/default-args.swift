// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test~partial.swiftmodule -module-name Test -primary-file %s
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/Test.swiftmodule %t/Test~partial.swiftmodule
// RUN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t | %FileCheck %s

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t.swiftinterface

public enum Enum {
  // CHECK: case pie(filling: String = "apple")
  case pie(filling: String = "apple")
}

public struct HasSubscript {
  // CHECK: subscript(x: Int = 0) -> Int {
  public subscript(x: Int = 0) -> Int { return 0 }
}

// CHECK: func hasClosureDefaultArg(_ x: () -> Void = {
// CHECK-NEXT: })
public func hasClosureDefaultArg(_ x: () -> Void = {
}) {
}

// CHECK: func hasMagicDefaultArgs(_ f: String = #file, _ fu: String = #function, _ l: Int = #line)
public func hasMagicDefaultArgs(_ f: String = #file, _ fu: String = #function, _ l: Int = #line) {}

// CHECK: func hasSimpleDefaultArgs(_ x: Int = 0, b: Int = 1)
public func hasSimpleDefaultArgs(_ x: Int = 0, b: Int = 1) {
}
