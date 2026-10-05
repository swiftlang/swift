// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name ThrowingNoexcept -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -emit-clang-header-path %t/throwing.h
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only %S/Inputs/throwing-noexcept.cpp -I %t -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public func freeFunction() throws {}
public struct Value {
  public let number: Int
  public init(_ number: Int) throws { self.number = number }
  public func read() throws -> Int { number }
  public mutating func update() throws {}
  public static func make() throws -> Int { 42 }
}
