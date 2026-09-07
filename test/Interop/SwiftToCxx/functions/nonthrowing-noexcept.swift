// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Noexcept -clang-header-expose-decls=all-public -typecheck -emit-clang-header-path %t/noexcept.h
// RUN: %target-interop-build-clangxx -std=c++14 -fsyntax-only %S/Inputs/nonthrowing-noexcept.cpp -I %t
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only %S/Inputs/nonthrowing-noexcept.cpp -I %t
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only %S/Inputs/nonthrowing-noexcept.cpp -I %t
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -fsyntax-only %S/Inputs/nonthrowing-noexcept.cpp -I %t

public func freeFunction() {}
public func identity<T>(_ value: T) -> T { value }

public struct Value {
  public var number: Int
  public init(_ number: Int) { self.number = number }
  public func read() -> Int { number }
  public mutating func increment() { number += 1 }
  public static func make() -> Value { Value(42) }
  public func identity<T>(_ value: T) -> T { value }
  public var isPositive: Bool { number > 0 }
  public static var answer: Int { 42 }
  public subscript(index: Int) -> Int { number + index }
}

public class Reference {
  public var number: Int = 0
  public init() {}
  public func read() -> Int { number }
  public static func make() -> Reference { Reference() }
  public subscript(index: Int) -> Int { number + index }
}
