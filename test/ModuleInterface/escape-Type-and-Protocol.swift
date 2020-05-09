// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s | %FileCheck %s

// CHECK: public let Type: Swift.Int
public let Type = 0

// CHECK: public struct A {
public struct A {
  // CHECK-NEXT: public struct `Type` {
  // CHECK-NEXT: }
  public struct `Type` {}
// CHECK-NEXT: }
}

// CHECK: public class B {
public class B {
  // CHECK-NEXT: public class `Type` {
  // CHECK: }
  public class `Type` {}

  // CHECK-NEXT: @_hasInitialValue public var `Type`: Swift.Int
  public var `Type` = 0
// CHECK: }
}

// CHECK: public struct C {
public struct C {
  // CHECK: public enum `Type` {
  public enum `Type` {
    // CHECK: }
  }
// CHECK-NEXT: }
}

// CHECK: public struct D {
public struct D {
  // CHECK: public typealias `Type` = Swift.Int
  public typealias `Type` = Int
// CHECK-NEXT: }
}

// CHECK: public protocol BestProtocol {
public protocol BestProtocol {
  // CHECK-NEXT: associatedtype `Type`
  associatedtype `Type`
// CHECK-NEXT: }
}

// CHECK: public enum CoolEnum {
public enum CoolEnum {
  // CHECK-NEXT: case `Type`
  case `Type`
  // CHECK-NEXT: case `Protocol`
  case `Protocol`
  // CHECK-NEXT: case `init`
  case `init`
  // CHECK-NEXT: case `self`
  case `self`

  // We allow Type and Protocol as method names, but we should still print them
  // escaped in case we tighten this restriction.
  // CHECK-NEXT: public func `Type`()
  public func Type() {}
  // CHECK-NEXT: public func `Protocol`()
  public func Protocol() {}
// CHECK: }
}

// CHECK: public enum UncoolEnum {
public enum UncoolEnum {
  // CHECK-NEXT: case `Type`, `Protocol`
  case `Type`, `Protocol`
// CHECK: }
}
