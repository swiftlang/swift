// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name execution_attr
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name execution_attr

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: concurrency

public struct Test {
  // CHECK:  nonisolated(nonsending) public init() async
  nonisolated(nonsending)
  public init() async {
  }

  // CHECK:  @concurrent public func test() async
  @concurrent
  public func test() async {
  }

  // CHECK:  public func other(_: nonisolated(nonsending) () async -> Swift.Void)
  public func other(_: nonisolated(nonsending) () async -> Void) {}

  // CHECK: nonisolated(nonsending) public var testOnVar: Swift.Int {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  nonisolated(nonsending)
  public var testOnVar: Int {
    get async {
      42
    }
  }

  // CHECK: nonisolated(nonsending) public subscript(onSubscript _: Swift.Int) -> Swift.Bool {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  nonisolated(nonsending)
  public subscript(onSubscript _: Int) -> Bool {
    get async {
      false
    }
  }
}

