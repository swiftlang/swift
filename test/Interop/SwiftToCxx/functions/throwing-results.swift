// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Results -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/results.h
// RUN: %FileCheck %s < %t/results.h
// RUN: %check-interop-cxx-header-in-clang(%t/results.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum ResultError: Error { case failure }
private func check(_ fail: Bool) throws {
  if fail { throw ResultError.failure }
}

private var livingCanaries = 0
public func canaryCount() -> Int { livingCanaries }
private final class Canary {
  init() { livingCanaries += 1 }
  deinit { livingCanaries -= 1 }
}

public struct Small {
  public var value: Int
  public init(_ value: Int) { self.value = value }
  public init(checked value: Int, _ fail: Bool) throws {
    try check(fail)
    self.value = value
  }
  public func doubled(_ fail: Bool) throws -> Small {
    try check(fail)
    return Small(value * 2)
  }
  public mutating func increment(_ fail: Bool) throws -> Int {
    try check(fail)
    value += 1
    return value
  }
  public static func make(_ fail: Bool) throws -> Small {
    try Small(checked: 42, fail)
  }
}

public struct Large {
  public let a, b, c, d, e: Int
  private let canary: Canary
  public init(_ fail: Bool) throws {
    // A failed initializer must clean up this partially initialized state.
    canary = Canary()
    try check(fail)
    a = 1; b = 2; c = 3; d = 4; e = 5
  }
}

public struct Holder {
  private let canary: Canary
  public init(_ fail: Bool) throws {
    try check(fail)
    canary = Canary()
  }
}

public final class Ref {
  public let value: Int
  public init(_ fail: Bool) throws {
    try check(fail)
    value = 42
  }
  public func result(_ fail: Bool) throws -> Small {
    try Small(checked: value, fail)
  }
}

public func direct(_ fail: Bool) throws -> Small {
  try Small(checked: 42, fail)
}
public func indirect(_ fail: Bool) throws -> Large { try Large(fail) }
public func reference(_ fail: Bool) throws -> Ref { try Ref(fail) }
public func string(_ fail: Bool) throws -> String {
  try check(fail)
  return "Hello from Swift"
}
public func generic<T>(_ value: T, _ fail: Bool) throws -> T {
  try check(fail)
  return value
}

// No Swift value may be materialized or destroyed on the error path.
// CHECK: swift::ThrowingResult<Small> direct
// CHECK: auto returnValue =
// CHECK: if (opaqueError != nullptr)
// CHECK: return {{.*}}returnNewValue

// CHECK: swift::ThrowingResult<T_0_0> generic
// CHECK: void *returnValue;
// CHECK: if (opaqueError != nullptr)
// CHECK: makeRetained(returnValue)
// CHECK: ::OpaqueStorage returnStorage
// CHECK: if (opaqueError != nullptr)
// CHECK: ::initializeWithTake

// CHECK: swift::ThrowingResult<Large> indirect
// CHECK: ::OpaqueStorage returnStorage
// CHECK: if (opaqueError != nullptr)
// CHECK: ::initializeWithTake

// CHECK: swift::ThrowingResult<Ref> reference
// CHECK: void *returnValue =
// CHECK: if (opaqueError != nullptr)
// CHECK: makeRetained(returnValue)
