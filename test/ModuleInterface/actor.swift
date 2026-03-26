// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Library.swiftinterface

// Re-verify with -swift-version 6
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -swift-version 6 -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Library.swiftinterface

// CHECK-NOT:     $Actors
// CHECK-LABEL:   @_hasMissingDesignatedInitializers public actor BasicActor {
public actor BasicActor {
  // CHECK:       {{@objc deinit|deinit}}
  // CHECK-NEXT:  @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency::UnownedSerialExecutor {
  // CHECK-NEXT:    get
  // CHECK-NEXT:  }
}

// CHECK-LABEL: public actor ActorWithConvenienceInitializers {
@available(SwiftStdlib 5.5, *)
public actor ActorWithConvenienceInitializers {
  private var x: Int

  // CHECK-NEXT: public convenience init(convenience x: Swift::Int)
  public init(convenience x: Int) {
    self.init(designated: x)
  }

  // CHECK-NEXT: public init()
  public init() {
    self.x = 0
  }

  // CHECK-NEXT: public init(designated x: Swift::Int)
  public init(designated x: Int) {
    self.x = x
  }

  // CHECK:       {{@objc deinit|deinit}}
  // CHECK-NEXT:  @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency::UnownedSerialExecutor {
  // CHECK-NEXT:    get
  // CHECK-NEXT:  }
}

// CHECK-LABEL: extension Library::ActorWithConvenienceInitializers {
@available(SwiftStdlib 5.5, *)
extension ActorWithConvenienceInitializers {
  // CHECK: public convenience init(convenienceInExtension x: Swift::Int)
  public init(convenienceInExtension x: Int) {
    self.init(designated: x)
  }
}
