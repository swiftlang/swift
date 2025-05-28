// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interfaces(%t/Library.swiftinterface, %t/Library.private.swiftinterface) %s -module-name Library -target %target-swift-5.3-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.private.swiftinterface) -module-name Library
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-PUBLIC < %t/Library.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-PRIVATE < %t/Library.private.swiftinterface

// REQUIRES: VENDOR=apple

// CHECK-NOT: #if compiler(>=5.3) && $Actors
// CHECK:     public actor ActorWithImplicitAvailability {
public actor ActorWithImplicitAvailability {
  // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.0, tvOS 13.0, watchOS 6.0, macOS 10.15, *)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}

// CHECK-NOT:  #if compiler(>=5.3) && $Actors
// CHECK:      @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
// CHECK-NEXT: public actor ActorWithExplicitAvailability {
@available(SwiftStdlib 5.2, *)
public actor ActorWithExplicitAvailability {
  // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}

// CHECK-NOT:  #if compiler(>=5.3) && $Actors
// CHECK:      @_hasMissingDesignatedInitializers @available(macOS, unavailable)
// CHECK-NEXT: public actor UnavailableActor {
@available(macOS, unavailable)
public actor UnavailableActor {
  // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  // CHECK-NEXT: @available(macOS, unavailable, introduced: 10.15)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}

// CHECK:      @_hasMissingDesignatedInitializers @available(*, deprecated, message: "Will be unavailable Swift 6")
// CHECK-NEXT: @available(swift, obsoleted: 6)
// CHECK-NEXT: public actor DeprecatedAndObsoleteInSwift6Actor {
@available(*, deprecated, message: "Will be unavailable Swift 6")
@available(swift, obsoleted: 6)
public actor DeprecatedAndObsoleteInSwift6Actor {
  // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.0, tvOS 13.0, watchOS 6.0, macOS 10.15, *)
  // CHECK-NEXT: @available(*, deprecated, message: "Will be unavailable Swift 6")
  // CHECK-NEXT: @available(swift, obsoleted: 6)
  // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
}

// CHECK: @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
// CHECK-NEXT: public enum Enum {
@available(SwiftStdlib 5.2, *)
public enum Enum {
  // CHECK-NOT:   #if compiler(>=5.3) && $Actors
  // CHECK:       @_hasMissingDesignatedInitializers public actor NestedActor {
  public actor NestedActor {
    // CHECK: @available(_SwiftToolchain 5.1, iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }
}

// CHECK: extension Library.Enum {
extension Enum {
  // CHECK-NOT: #if compiler(>=5.3) && $Actors
  // CHECK:     @_hasMissingDesignatedInitializers public actor ExtensionNestedActor {
  public actor ExtensionNestedActor {
    // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }

  // CHECK-NOT:  #if compiler(>=5.3) && $Actors
  // CHECK:      @_hasMissingDesignatedInitializers @available(macOS, unavailable)
  // CHECK-NEXT: public actor UnavailableExtensionNestedActor {
  @available(macOS, unavailable)
  public actor UnavailableExtensionNestedActor {
    // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
    // CHECK-NEXT: @available(macOS, unavailable, introduced: 10.15.4)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
    // CHECK-NEXT:   get
    // CHECK-NEXT: }
  }
}

// CHECK-PUBLIC:      @available(macOS, unavailable)
// CHECK-PUBLIC-NEXT: @available(iOS, unavailable)
// CHECK-PUBLIC-NEXT: @available(watchOS, unavailable)
// CHECK-PUBLIC-NEXT: @available(tvOS, unavailable
// CHECK-PUBLIC-NEXT: public struct SPIAvailableStruct
// CHECK-PRIVATE: @_spi_available(macOS, introduced: 10.15.4)
// CHECK-PRIVATE-NEXT: @_spi_available(iOS, introduced: 13.4)
// CHECK-PRIVATE-NEXT: @_spi_available(watchOS, introduced: 6.2)
// CHECK-PRIVATE-NEXT: @_spi_available(tvOS, introduced: 13.4)
// CHECK-PRIVATE-NEXT: public struct SPIAvailableStruct
@_spi_available(SwiftStdlib 5.2, *)
public struct SPIAvailableStruct {
  // CHECK-NOT:  #if compiler(>=5.3) && $Actors
  // CHECK:      @_hasMissingDesignatedInitializers @available(macOS, unavailable)
  // CHECK-NEXT: public actor UnavailableNestedActor
  @available(macOS, unavailable)
  public actor UnavailableNestedActor {
    // CHECK-PUBLIC:      @available(iOS, unavailable)
    // CHECK-PUBLIC-NEXT: @available(tvOS, unavailable)
    // CHECK-PUBLIC-NEXT: @available(watchOS, unavailable)
    // CHECK-PUBLIC-NEXT: @available(macOS, unavailable)
    // CHECK-PUBLIC-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
    // CHECK-PRIVATE: @_spi_available(iOS, introduced: 13.4)
    // CHECK-PRIVATE-NEXT: @_spi_available(tvOS, introduced: 13.4)
    // CHECK-PRIVATE-NEXT: @_spi_available(watchOS, introduced: 6.2)
    // CHECK-PRIVATE-NEXT: @_spi_available(macOS, unavailable, introduced: 10.15.4)
    // CHECK-PRIVATE-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
  }
}

// CHECK:      @_hasMissingDesignatedInitializers @available(macCatalyst 13.1, *)
// CHECK-NEXT: public class MacCatalystAvailableClass
@available(macCatalyst 13.1, *)
public class MacCatalystAvailableClass {
  // CHECK-NOT: #if compiler(>=5.3) && $Actors
  // CHECK:     @_hasMissingDesignatedInitializers public actor NestedActor
  public actor NestedActor {
    // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.0, tvOS 13.0, watchOS 6.0, macOS 10.15, macCatalyst 13.1, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
  }

  // CHECK-NOT:  #if compiler(>=5.3) && $Actors
  // CHECK:      @_hasMissingDesignatedInitializers @available(macCatalyst 14, *)
  // CHECK-NEXT: public actor LessAvailableMacCatalystActor
  @available(macCatalyst 14, *)
  public actor LessAvailableMacCatalystActor {
    // CHECK:      @available(_SwiftToolchain 5.1, iOS 13.0, tvOS 13.0, watchOS 6.0, macOS 10.15, macCatalyst 14, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
  }

  // CHECK-NOT: #if compiler(>=5.3) && $Actors
  // CHECK:     @_hasMissingDesignatedInitializers @available(iOS 15.0, macOS 12.0, *)
  // CHECK-NEXT: public actor AvailableiOSAndMacOSNestedActor {
  @available(iOS 15.0, macOS 12.0, *)
  public actor AvailableiOSAndMacOSNestedActor {
    // CHECK:      @available(_SwiftToolchain 5.1, iOS 15.0, tvOS 13.0, watchOS 6.0, macOS 12.0, *)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
  }

  // CHECK-NOT:  #if compiler(>=5.3) && $Actors
  // CHECK:      @_hasMissingDesignatedInitializers @available(iOS, unavailable)
  // CHECK-NEXT: public actor UnavailableiOSNestedActor
  @available(iOS, unavailable)
  public actor UnavailableiOSNestedActor {
    // CHECK:      @available(_SwiftToolchain 5.1, tvOS 13.0, watchOS 6.0, macOS 10.15, *)
    // CHECK-NEXT: @available(iOS, unavailable, introduced: 13.0)
    // CHECK-NEXT: @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor
  }
}
