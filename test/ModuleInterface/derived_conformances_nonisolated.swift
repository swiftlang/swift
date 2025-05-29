// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -disable-availability-checking -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -disable-availability-checking -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: concurrency

// CHECK: @_Concurrency.MainActor public struct X1 : Swift.Equatable, Swift.Hashable, Swift.Codable
@MainActor public struct X1: Equatable, Hashable, Codable {
  let x: Int
  let y: String

  // CHECK: nonisolated public static func == (a: Library.X1, b: Library.X1) -> Swift.Bool
  // CHECK: nonisolated public func encode(to encoder: any Swift.Encoder) throws
  // CHECK: nonisolated public func hash(into hasher: inout Swift.Hasher)
  // CHECK: nonisolated public var hashValue: Swift.Int
  // CHECK: nonisolated public init(from decoder: any Swift.Decoder) throws
}
