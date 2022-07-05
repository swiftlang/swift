// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test -enable-experimental-bound-generic-extensions
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

public struct Tree<T> {
  public struct Branch<B> {
    public struct Nest<N> {
      public struct Egg {}
    }
  }
}

// CHECK: extension Test.Tree.Branch.Nest.Egg {
// CHECK:   public static func tweet()
// CHECK: }
extension Tree.Branch.Nest.Egg { public static func tweet() {} }

// CHECK: extension Test.Tree.Branch.Nest.Egg where T == Swift.Int {
// CHECK:   public static func twoot()
// CHECK: }
extension Tree<Int>.Branch.Nest.Egg { public static func twoot() {} }

// CHECK: extension Test.Tree.Branch.Nest.Egg where T == Swift.Int, B == Swift.String {
// CHECK:   public static func twote()
// CHECK: }
extension Tree<Int>.Branch<String>.Nest.Egg { public static func twote() {} }

// CHECK: extension Test.Tree.Branch.Nest.Egg where T == Swift.Int, B == Swift.String, N == () {
// CHECK:   public static func twite()
// CHECK: }
extension Tree<Int>.Branch<String>.Nest<Void>.Egg { public static func twite() {} }

// CHECK: extension Swift.Array where Element == Swift.String {
// CHECK:   public func rejoinder() -> Swift.String
// CHECK: }
extension [String] { public func rejoinder() -> String { return self.joined() } }

// CHECK: public typealias StringDict<T> = [Swift.String : T]
public typealias StringDict<T> = [String: T]

// CHECK: extension Swift.Dictionary where Key == Swift.String, Value == Swift.Int
extension StringDict<Int> { public static func mark() {} }
