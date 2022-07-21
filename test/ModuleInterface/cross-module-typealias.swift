// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/Original.swiftinterface) %t/Original.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Original.swiftinterface)

// RUN: %target-swift-emit-module-interface(%t/Aliases.swiftinterface) %t/Aliases.swift -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Aliases.swiftinterface) -I %t

// RUN: %target-swift-emit-module-interface(%t/UsesAliases.swiftinterface) %t/UsesAliases.swift -I %t -disable-availability-checking
// TODO: enable verification of UsesAliases.swiftinterface (rdar://91447971)
// RUN/: %target-swift-typecheck-module-from-interface(%t/UsesAliases.swiftinterface) -I %t

// RUN: %FileCheck %s < %t/UsesAliases.swiftinterface


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


//--- UsesAliases.swift

import Aliases

// CHECK: public class InheritsFromClazzAlias : Aliases.ClazzAlias
public class InheritsFromClazzAlias: ClazzAlias {}

// CHECK: public protocol HasAssociatedTypeWithStructAliasDefault
public protocol HasAssociatedTypeWithStructAliasDefault {
  // CHECK: associatedtype Assoc = Aliases.StructAlias
  associatedtype Assoc = StructAlias
}

// CHECK: public func usesStructAlias(_ x: Aliases.StructAlias) -> Aliases.StructAlias
public func usesStructAlias(_ x: StructAlias) -> StructAlias {
  return x
}

// CHECK: public func usesGenericTypesConstrainedToProtoAlias<T>(_ x: T) -> T where T : Original.Proto
public func usesGenericTypesConstrainedToProtoAlias<T: ProtoAlias>(_ x: T) -> T {
  return x
}

// CHECK: public func usesGenericTypesConstrainedToProtoAliasWithWhereClause<T>(_ x: T) -> T where T : Original.Proto
public func usesGenericTypesConstrainedToProtoAliasWithWhereClause<T>(_ x: T) -> T where T: ProtoAlias {
  return x
}

// TODO: opaque parameters should probably print fully qualified
// CHECK: public func usesOpaqueProtoAliasTypes(_ x: some ProtoAlias) -> some Original.Proto
public func usesOpaqueProtoAliasTypes(_ x: some ProtoAlias) -> some ProtoAlias {
  return x
}

// CHECK: public func usesExistentialProtoAliasTypes(_ x: any Original.Proto) -> any Original.Proto
public func usesExistentialProtoAliasTypes(_ x: any ProtoAlias) -> any ProtoAlias {
  return x
}

// CHECK: public struct ConformsToProtoAlias : Aliases.ProtoAlias
public struct ConformsToProtoAlias: ProtoAlias {
  // CHECK: @_implements(Aliases.ProtoAlias, requirement) public func requirement()
  @_implements(ProtoAlias, requirement) public func requirement() {}
}

// CHECK: public struct HasProtoAliasGenericConstraint<T> where T : Original.Proto
public struct HasProtoAliasGenericConstraint<T: ProtoAlias> {}

// CHECK: public struct HasMembers
public struct HasMembers {
  // CHECK: public var foo: Aliases.StructAlias
  public var foo: StructAlias
  
  // CHECK: public var fooInferred: Aliases.StructAlias
  public var fooInferred = StructAlias()
  
  // CHECK: public var closure: (Aliases.StructAlias) -> Aliases.StructAlias
  public var closure: (StructAlias) -> StructAlias
  
  // CHECK: public var closureInferred: (_ x: Aliases.StructAlias) -> Aliases.StructAlias
  public var closureInferred = { (x: StructAlias) -> StructAlias in
    return x
  }
  
  // TODO: referencing StructAlias here results in a spurious warning:
  // warning: cannot use struct 'Struct' here; 'Original' was not imported by this file
  // CHECK: public var tuple: (Aliases.StructAlias)
  public var tuple: (StructAlias)
  
  // CHECK: public subscript(i: Aliases.StructAlias) -> Aliases.StructAlias
  public subscript(i: StructAlias) -> StructAlias {
    return i
  }
  
  // CHECK: @Original.Wrapper public var wrapped: Swift.Int
  @WrapperAlias public var wrapped: Int
}

// CHECK: public enum HasCasePayloads
public enum HasCasePayloads {
  // CHECK: case hasStructAlias(Aliases.StructAlias)
  case hasStructAlias(StructAlias)
  
  // CHECK: case hasExistentialProtoAlias(any Original.Proto)
  case hasExistentialProtoAlias(any ProtoAlias)
}

// CHECK: extension Original.Struct
extension StructAlias {
  // CHECK-NEXT: public func publicMember()
  public func publicMember() {}
}

// CHECK: public typealias ProtoAliasAlias = Aliases.ProtoAlias
public typealias ProtoAliasAlias = ProtoAlias

// TODO: @_typeEraser should probably print fully qualified
// CHECK: @_typeEraser(StructAlias) public protocol StructAliasTypeEraser
@_typeEraser(StructAlias) public protocol StructAliasTypeEraser {}

// CHECK: extension Original.Struct : UsesAliases.StructAliasTypeEraser
extension StructAlias: StructAliasTypeEraser {
  // CHECK: public init<T>(erasing: T) where T : UsesAliases.StructAliasTypeEraser
  public init<T: StructAliasTypeEraser>(erasing: T) {}
}
