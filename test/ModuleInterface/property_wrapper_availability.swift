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
}

// CHECK: public struct HasWrappers {
public struct HasWrappers {
  // CHECK: @available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *)
  // CHECK-NEXT: @Library.ConditionallyAvailableWrapper public var x: Swift.Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT:   @available(iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
  // CHECK-NEXT:   set
  // CHECK-NEXT:   @available(iOS 13.4, tvOS 13.4, watchOS 6.2, macOS 10.15.4, *)
  // CHECK-NEXT:   _modify
  // CHECK-NEXT: }
  @available(SwiftStdlib 5.2, *)
  @ConditionallyAvailableWrapper public var x: Int
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
  // CHECK-LABEL:   @Library.UnavailableWrapper public var x: Swift.Int {
  // CHECK-NEXT:      get
  // CHECK-NEXT:      @available(iOS, unavailable)
  // CHECK-NEXT:      @available(tvOS, unavailable)
  // CHECK-NEXT:      @available(watchOS, unavailable)
  // CHECK-NEXT:      @available(macOS, unavailable)
  // CHECK-NEXT:      set
  // CHECK-NEXT:      @available(iOS, unavailable)
  // CHECK-NEXT:      @available(tvOS, unavailable)
  // CHECK-NEXT:      @available(watchOS, unavailable)
  // CHECK-NEXT:      @available(macOS, unavailable)
  // CHECK-NEXT:      _modify
  // CHECK-NEXT:    }
  @UnavailableWrapper public var x: Int
}
