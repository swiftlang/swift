// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/Original.swiftinterface) %t/Original.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Original.swiftinterface)

// RUN: %target-swift-emit-module-interface(%t/Aliases.swiftinterface) %t/Aliases.swift -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Aliases.swiftinterface) -I %t

// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesNoImport.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesImplementationOnlyImport.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/UsesAliasesWithImport.swift  -I %t


//--- Original.swift

open class Clazz {}

public protocol Proto {
  func requirement()
}

public struct Struct {
  public init() {}
}

@propertyWrapper
public struct Wrapper<T> {
  public var wrappedValue: T
  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}


//--- Aliases.swift

import Original

public typealias ClazzAlias = Clazz
public typealias ProtoAlias = Proto
public typealias StructAlias = Struct
public typealias WrapperAlias = Wrapper


//--- UsesAliasesNoImport.swift

import Aliases

// expected-warning@+1 {{'ClazzAlias' aliases 'Original.Clazz' and cannot be used here because 'Original' was not imported by this file; this is an error in Swift 6}}
public class InheritsFromClazzAlias: ClazzAlias {}

@inlinable public func inlinableFunc() {
  // expected-warning@+1 {{'StructAlias' aliases 'Original.Struct' and cannot be used in an '@inlinable' function because 'Original' was not imported by this file; this is an error in Swift 6}}
  _ = StructAlias.self
}

// expected-warning@+1 {{'ProtoAlias' aliases 'Original.Proto' and cannot be used here because 'Original' was not imported by this file; this is an error in Swift 6}}
public func takesGeneric<T: ProtoAlias>(_ t: T) {}

public struct HasMembers {
  // expected-warning@+1 {{'WrapperAlias' aliases 'Original.Wrapper' and cannot be used as property wrapper here because 'Original' was not imported by this file; this is an error in Swift 6}}
  @WrapperAlias public var wrapped: Int
}

// expected-warning@+1 {{'StructAlias' aliases 'Original.Struct' and cannot be used in an extension with public or '@usableFromInline' members because 'Original' was not imported by this file; this is an error in Swift 6}}
extension StructAlias {
  public func someFunc() {}
}


//--- UsesAliasesImplementationOnlyImport.swift

import Aliases
@_implementationOnly import Original

@inlinable public func inlinableFunc() {
  // expected-warning@+1 {{'StructAlias' aliases 'Original.Struct' and cannot be used in an '@inlinable' function because 'Original' has been imported as implementation-only; this is an error in Swift 6}}
  _ = StructAlias.self
}

// expected-warning@+1 {{'ProtoAlias' aliases 'Original.Proto' and cannot be used here because 'Original' has been imported as implementation-only; this is an error in Swift 6}}
public func takesGeneric<T: ProtoAlias>(_ t: T) {}


//--- UsesAliasesWithImport.swift

import Aliases
import Original

@inlinable public func inlinableFunc() {
  _ = StructAlias.self
}

public func takesGeneric<T: ProtoAlias>(_ t: T) {}
