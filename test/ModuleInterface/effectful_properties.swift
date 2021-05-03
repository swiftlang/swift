// RUN: %target-swift-frontend -enable-experimental-concurrency -typecheck -swift-version 5 -enable-library-evolution -emit-module-interface-path %t.swiftinterface %s -module-name EffProps
// RUN: %FileCheck %s < %t.swiftinterface

public struct MyStruct {}

// CHECK-LABEL: public var status
// CHECK: get async throws

public extension MyStruct {
  struct InnerStruct {
      public var status: Bool { get async throws { false } }
    }
}

// CHECK-LABEL: public var hello
// CHECK: get async

// CHECK-LABEL: public subscript
// CHECK: get async throws

public class C {
  public var hello: Int { get async { 0 } }

  public subscript(_ x: Int) -> Void {
    get async throws { }
  }
}

// CHECK-LABEL: public var world
// CHECK: get throws

public enum E {
  public var world: Int { get throws { 0 } }
}