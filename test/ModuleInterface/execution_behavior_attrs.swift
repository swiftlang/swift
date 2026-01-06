// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name execution_attr
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name execution_attr

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: concurrency

public struct TestWithAttrs {
  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: public func test(_: nonisolated(nonsending) @escaping () async -> Swift.Void)
  // CHECK-NEXT: #endif
  public func test(_: nonisolated(nonsending) @escaping () async -> Void) {}

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: public func withInOut(fn: nonisolated(nonsending) inout () async -> Swift.Void)
  // CHECK-NEXT: #endif
  public func withInOut(fn: nonisolated(nonsending) inout () async -> Void) {}
}

public struct Test {
  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: nonisolated(nonsending) public init() async
  // CHECK-NEXT: #endif
  nonisolated(nonsending)
  public init() async {
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: @concurrent public init(something: Swift.Int) async
  // CHECK-NEXT: #endif
  @concurrent
  public init(something: Int) async {
  }

  // CHECK-NOT: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK: public init(noExplicit: Swift.String) async
  // CHECK-NOT: #endif
  public init(noExplicit: String) async {
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT:  @concurrent public func test() async
  // CHECK-NEXT: #endif
  @concurrent
  public func test() async {
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: public func other(_: nonisolated(nonsending) () async -> Swift.Void)
  // CHECK-NEXT: #endif
  public func other(_: nonisolated(nonsending) () async -> Void) {}

  // CHECK-NOT: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK: public func concurrentResult(_: () async -> Swift.Void) -> (Swift.Int) async -> Swift.Void
  // CHECK-NOT: #endif
  public func concurrentResult(_: () async -> Void) -> @concurrent (Int) async -> Void {}

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: public func nestedPositions1(_: Swift.Array<[Swift.String : nonisolated(nonsending) (Swift.Int) async -> Swift.Void]>)
  // CHECK-NEXT: #endif
  public func nestedPositions1(_: Array<[String: nonisolated(nonsending) (Int) async -> Void]>) {}

  // CHECK-NOT: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK: public func nestedPositions2(_: Swift.Array<[Swift.String : (Swift.Int) async -> Swift.Void]>)
  // CHECK-NOT: #endif
  public func nestedPositions2(_: Array<[String: @concurrent (Int) async -> Void]>) {}

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: nonisolated(nonsending) public var testOnVar: Swift.Int {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  // CHECK-NEXT: #endif
  nonisolated(nonsending)
  public var testOnVar: Int {
    get async {
      42
    }
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: @concurrent public var testOnVarConcurrent: Swift.Int {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  // CHECK-NEXT: #endif
  @concurrent
  public var testOnVarConcurrent: Int {
    get async {
      42
    }
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: nonisolated(nonsending) public subscript(onSubscript _: Swift.Int) -> Swift.Bool {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  // CHECK-NEXT: #endif
  nonisolated(nonsending)
  public subscript(onSubscript _: Int) -> Bool {
    get async {
      false
    }
  }

  // CHECK: #if compiler(>=5.3) && $AsyncExecutionBehaviorAttributes
  // CHECK-NEXT: @concurrent public subscript(concurrentOnSubscript _: Swift.Int) -> Swift.Bool {
  // CHECK-NEXT:   get async
  // CHECK-NEXT: }
  // CHECK-NEXT: #endif
  @concurrent
  public subscript(concurrentOnSubscript _: Int) -> Bool {
    get async {
      false
    }
  }
}

