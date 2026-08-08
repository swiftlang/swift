// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/FooProtocol.swiftmodule -emit-objc-header-path %t/FooProtocol.h -module-name FooProtocol %t/FooProtocol.swift
// RUN: %target-swift-frontend -typecheck -sdk %sdk -I %t %t/main.swift

//--- FooProtocol.swift
import Foundation

@objc public protocol FooProtocol {
  func instanceMethod() -> Bool
  static func classMethod() -> Bool

  func thisInstanceMethod(_ number: Int, hasArgs: String)
  static func thisClassMethod(_ number: Int, hasArgs: String)

  func someThrowingNoArgMethod() throws -> String
  func someThrowingMethod(withAnArg: Int) throws -> String
  func someAsyncNoArgMethod() async -> String
  func someAsyncMethod(withAnArg: Int) async -> String
  func someThrowingAsyncNoArgMethod() async throws -> String
  func someThrowingAsyncMethod(withAnArg: Int) async throws -> String

  var instanceProperty: Int { get set }
  static var staticProperty: Int { get set }
}

//--- FooImpl.h
#import <Foundation/Foundation.h>
#import "FooProtocol.h"

@interface FooImpl : NSObject <FooProtocol>
@end

//--- module.modulemap
module FooImpl {
  header "FooImpl.h"
  export *
}

//--- main.swift
import FooProtocol
import FooImpl

func testMirroring(obj: FooImpl) async throws {
  _ = obj.instanceMethod()
  _ = FooImpl.classMethod()

  obj.thisInstanceMethod(1, hasArgs: "hello")
  FooImpl.thisClassMethod(1, hasArgs: "hello")

  _ = obj.instanceProperty
  obj.instanceProperty = 10

  _ = FooImpl.staticProperty
  FooImpl.staticProperty = 10

  _ = try obj.someThrowingNoArgMethod()
  _ = try obj.someThrowingMethod(withAnArg: 1)
  _ = await obj.someAsyncNoArgMethod()
  _ = await obj.someAsyncMethod(withAnArg: 1)
  _ = try await obj.someThrowingAsyncNoArgMethod()
  _ = try await obj.someThrowingAsyncMethod(withAnArg: 1)
}
