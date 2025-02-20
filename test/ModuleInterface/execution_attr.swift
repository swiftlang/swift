// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name execution_attr -enable-experimental-feature ExecutionAttribute
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name execution_attr

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: swift_feature_ExecutionAttribute

public struct Test {
  // CHECK:  #if compiler(>=5.3) && $ExecutionAttribute
  // CHECK-NEXT:  @execution(concurrent) public func test() async
  // CHECK-NEXT:  #else
  // CHECK-NEXT:  public func test() async
  // CHECK-NEXT:  #endif
  @execution(concurrent)
  public func test() async {
  }

  // CHECK:  #if compiler(>=5.3) && $ExecutionAttribute
  // CHECK-NEXT:  public func other(_: @execution(caller) () async -> Swift.Void)
  // CHECK-NEXT:  #else
  // CHECK-NEXT:  public func other(_: () async -> Swift.Void)
  // CHECK-NEXT:  #endif
  public func other(_: @execution(caller) () async -> Void) {}
}

