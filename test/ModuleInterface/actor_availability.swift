// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -target %target-swift-abi-5.3-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: VENDOR=apple

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: public actor ActorWithImplicitAvailability {
public actor ActorWithImplicitAvailability {
  // CHECK: @available(iOS 13.0, tvOS 13.0, watchOS 6.0, macOS 10.15, *)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}
// CHECK: #endif

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
// CHECK-NEXT: public actor ActorWithExplicitAvailability {
@available(SwiftStdlib 5.2, *)
public actor ActorWithExplicitAvailability {
  // CHECK: @available(iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}
// CHECK: #endif

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: @_hasMissingDesignatedInitializers @available(macOS, unavailable)
// CHECK-NEXT: public actor UnavailableActor {
@available(macOS, unavailable)
public actor UnavailableActor {
  // CHECK: @available(iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  // CHECK-NEXT: @available(macOS, unavailable)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}
// CHECK: #endif

// CHECK: @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
// CHECK-NEXT: public enum Enum {
@available(SwiftStdlib 5.2, *)
public enum Enum {
  // CHECK:   #if compiler(>=5.3) && $Actors
  // CHECK-NEXT: @_hasMissingDesignatedInitializers public actor NestedActor {
  public actor NestedActor {
    // CHECK: @available(iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }
  // CHECK: #endif
}

// CHECK: extension Library.Enum {
extension Enum {
  // CHECK: #if compiler(>=5.3) && $Actors
  // CHECK-NEXT: @_hasMissingDesignatedInitializers public actor ExtensionNestedActor {
  public actor ExtensionNestedActor {
    // CHECK: @available(iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }

  // CHECK: #if compiler(>=5.3) && $Actors
  // CHECK-NEXT: @_hasMissingDesignatedInitializers @available(macOS, unavailable)
  // CHECK-NEXT: public actor UnavailableExtensionNestedActor {
  @available(macOS, unavailable)
  public actor UnavailableExtensionNestedActor {
    // CHECK: @available(iOS 13.4, tvOS 13.4, watchOS 6.2, *)
    // CHECK-NEXT: @available(macOS, unavailable)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }
}
