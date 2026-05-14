// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/TestResilient.swiftinterface) %s -module-name TestResilient
// RUN: %target-swift-typecheck-module-from-interface(%t/TestResilient.swiftinterface) -module-name TestResilient
// RUN: %FileCheck %s < %t/TestResilient.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/TestResilient.swiftinterface -o %t/TestResilient.swiftmodule

// CHECK: @propertyWrapper public struct WrapperWithInternalSet<T> {
// CHECK:   public var wrappedValue: T
// CHECK:   public var projectedValue: Swift::Bool {
// CHECK:     get
// CHECK:   }
// CHECK:   public init(wrappedValue: T)
// CHECK: }
@propertyWrapper
public struct WrapperWithInternalSet<T> {
  public var wrappedValue: T
  public internal(set) var projectedValue: Bool = true
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

// CHECK: @propertyWrapper public struct WrapperWithPrivateSet<T> {
// CHECK:   public var wrappedValue: T
// CHECK:   public var projectedValue: Swift::Bool {
// CHECK:     get
// CHECK:   }
// CHECK:   public init(wrappedValue: T)
// CHECK: }
@propertyWrapper
public struct WrapperWithPrivateSet<T> {
  public var wrappedValue: T
  public private(set) var projectedValue: Bool = true
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

// CHECK: @propertyWrapper public struct WrapperWithImmutable<T> {
// CHECK:   public var wrappedValue: T
// CHECK:   public let projectedValue: Swift::Bool
// CHECK:   public init(wrappedValue: T)
// CHECK: }
@propertyWrapper
public struct WrapperWithImmutable<T> {
  public var wrappedValue: T
  public let projectedValue: Bool = true
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

// CHECK: @propertyWrapper public struct WrapperWithComputedPublic<T> {
// CHECK:   public var wrappedValue: T
// CHECK:   public var projectedValue: Swift::Bool {
// CHECK:     get
// CHECK:     set
// CHECK:   }
// CHECK: }
@propertyWrapper
public struct WrapperWithComputedPublic<T> {
  public var wrappedValue: T
  public var projectedValue: Bool {
    get { false }
    set { }
  }
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

// CHECK: @propertyWrapper public struct WrapperWithComputedImmutable<T> {
// CHECK:   public var wrappedValue: T
// CHECK:   public var projectedValue: Swift::Bool {
// CHECK:     get
// CHECK:   }
// CHECK:   public init(wrappedValue: T)
// CHECK: }
@propertyWrapper
public struct WrapperWithComputedImmutable<T> {
  public var wrappedValue: T
  public var projectedValue: Bool {
    get { false }
  }
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

public struct Test {
  // CHECK: public var $v1: Swift::Bool {
  // CHECK:   get
  // CHECK: }
  @WrapperWithInternalSet public var v1: Int

  // CHECK: public var $v2: Swift::Bool {
  // CHECK:   get
  // CHECK: }
  @WrapperWithPrivateSet public var v2: String

  // CHECK: public var $v3: Swift::Bool {
  // CHECK:   get
  // CHECK: }
  @WrapperWithImmutable public var v3: Bool

  // CHECK: public var $v4: Swift::Bool {
  // CHECK:   get
  // CHECK:   set
  // CHECK: }
  @WrapperWithComputedPublic public var v4: Double

  // CHECK: public var $v5: Swift::Bool {
  // CHECK:   get
  // CHECK: }
  @WrapperWithComputedImmutable public var v5: Int?

  // CHECK: public var $v6: Swift::Bool {
  // CHECK:   get
  // CHECK: }
  @WrapperWithComputedPublic public private(set) var v6: Bool?
}
