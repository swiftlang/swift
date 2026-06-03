// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library -target %target-swift-5.3-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

@available(SwiftStdlib 5.2, *)
@propertyWrapper
public struct ConditionallyAvailableWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: T { wrappedValue }
}

@available(SwiftStdlib 5.2, *)
@propertyWrapper
public struct ConditionallyAvailableMutableProjectionWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: T {
    get { wrappedValue }
    set { wrappedValue = newValue }
  }
}

@available(SwiftStdlib 5.3, *)
@propertyWrapper
public struct StricterProjectionWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  @available(SwiftStdlib 5.5, *)
  public var projectedValue: T { wrappedValue }
}

@available(macOS, unavailable)
@available(iOS, unavailable)
@available(watchOS, unavailable)
@available(tvOS, unavailable)
@propertyWrapper
public struct UnavailableWrapper<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: T { wrappedValue }
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
  // CHECK:      @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
  // CHECK-NEXT: @Library::ConditionallyAvailableWrapper<Swift::Int> @_projectedValueProperty($conditionallyAvailable) public var conditionallyAvailable: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT:   @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT:   set
  // CHECK-NEXT:   @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  // CHECK-NEXT: @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT: public var $conditionallyAvailable: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  @available(SwiftStdlib 5.2, *)
  @ConditionallyAvailableWrapper public var conditionallyAvailable: Int

  // CHECK:      @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
  // CHECK-NEXT: @Library::ConditionallyAvailableMutableProjectionWrapper<Swift::Int> @_projectedValueProperty($mutableProjection) public var mutableProjection: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT:   @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT:   set
  // CHECK-NEXT:   @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  // CHECK-NEXT: @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT: public var $mutableProjection: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT:   @available(macOS 10.15.4, iOS 13.4, tvOS 13.4, watchOS 6.2, *)
  // CHECK-NEXT:   set
  // CHECK-NEXT: }
  @available(SwiftStdlib 5.2, *)
  @ConditionallyAvailableMutableProjectionWrapper public var mutableProjection: Int

  // CHECK:      @available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
  // CHECK-NEXT: @Library::StricterProjectionWrapper<Swift::Int> @_projectedValueProperty($stricterProjection) public var stricterProjection: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT:   @available(macOS 11.0, iOS 14.0, tvOS 14.0, watchOS 7.0, *)
  // CHECK-NEXT:   set
  // CHECK-NEXT:   @available(macOS 11.0, iOS 14.0, tvOS 14.0, watchOS 7.0, *)
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  // CHECK-NEXT: @available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *)
  // CHECK-NEXT: public var $stricterProjection: Swift::Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  @available(SwiftStdlib 5.3, *)
  @StricterProjectionWrapper public var stricterProjection: Int
}

// CHECK:       @available(macOS, unavailable)
// CHECK-NEXT:  @available(iOS, unavailable)
// CHECK-NEXT:  @available(watchOS, unavailable)
// CHECK-NEXT:  @available(tvOS, unavailable)
// CHECK-NEXT:  public struct UnavailableHasWrappers {
@available(macOS, unavailable)
@available(iOS, unavailable)
@available(watchOS, unavailable)
@available(tvOS, unavailable)
public struct UnavailableHasWrappers {
  // CHECK-LABEL:   @Library::UnavailableWrapper<Swift::Int> @_projectedValueProperty($unavailable) public var unavailable: Swift::Int {
  // CHECK-NEXT:      get
  // CHECK-NEXT:      @available(macOS, unavailable)
  // CHECK-NEXT:      @available(iOS, unavailable)
  // CHECK-NEXT:      @available(tvOS, unavailable)
  // CHECK-NEXT:      @available(watchOS, unavailable)
  // CHECK-NEXT:      set
  // CHECK-NEXT:      @available(macOS, unavailable)
  // CHECK-NEXT:      @available(iOS, unavailable)
  // CHECK-NEXT:      @available(tvOS, unavailable)
  // CHECK-NEXT:      @available(watchOS, unavailable)
  // CHECK-NEXT:      _modify
  // CHECK-NEXT:    }
  // CHECK-NEXT:    @available(macOS, unavailable)
  // CHECK-NEXT:    @available(iOS, unavailable)
  // CHECK-NEXT:    @available(tvOS, unavailable)
  // CHECK-NEXT:    @available(watchOS, unavailable)
  // CHECK-NEXT:    public var $unavailable: Swift::Int {
  // CHECK-NEXT:      get
  // CHECK-NEXT:    }
  @UnavailableWrapper public var unavailable: Int
}
