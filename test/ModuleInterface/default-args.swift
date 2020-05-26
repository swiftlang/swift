// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test~partial.swiftmodule -module-name Test -primary-file %s
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/Test.swiftmodule %t/Test~partial.swiftmodule
// RUN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t -prefer-type-repr=false -fully-qualified-types=true | %FileCheck %s

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Test.swiftinterface -module-name Test -enable-library-evolution %s
// RUN: rm %t/Test.swiftmodule
// RUN: echo "import Test" > %t/test-client.swift
// RUN: %target-swift-frontend -typecheck -I%t %t/test-client.swift
// RUN: %FileCheck %s < %t/Test.swiftinterface

// CHECK: class Base {
public class Base {
  // CHECK: init(x: Swift.Int = 3)
  public init(x: Int = 3) {}
  public convenience init(convInit: Int) {
    self.init(x: convInit)
  }
  // CHECK: foo(y: Swift.Int = 42)
  public func foo(y: Int = 42) {}
}

// CHECK: class Derived : Test.Base {
public class Derived: Base {
  // CHECK: init(y: Swift.Int)
  public convenience init(y: Int) {
    self.init()
  }

  // CHECK-NOT: init(convInit: Swift.Int = super)
  // CHECK: override {{(public )?}}init(x: Swift.Int = super)
  // CHECK-NOT: init(convInit: Swift.Int = super)
}

public enum Enum {
  // CHECK: case pie(filling: Swift.String = "apple")
  case pie(filling: String = "apple")
}

public struct HasSubscript {
  // CHECK: subscript(x: Swift.Int = 0) -> Swift.Int {
  public subscript(x: Int = 0) -> Int { return 0 }
}

// CHECK: func hasClosureDefaultArg(_ x: () -> Swift.Void = {
// CHECK-NEXT: })
public func hasClosureDefaultArg(_ x: () -> Void = {
}) {
}

// CHECK: func hasMagicDefaultArgs(_ f: Swift.String = #file, _ fu: Swift.String = #function, _ l: Swift.Int = #line)
public func hasMagicDefaultArgs(_ f: String = #file, _ fu: String = #function, _ l: Int = #line) {}

// CHECK: func hasSimpleDefaultArgs(_ x: Swift.Int = 0, b: Swift.Int = 1)
public func hasSimpleDefaultArgs(_ x: Int = 0, b: Int = 1) {
}
