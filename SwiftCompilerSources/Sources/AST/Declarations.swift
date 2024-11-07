//===--- Declarations.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import ASTBridging

/// The base class for all declarations in Swift.
@_semantics("arc.immortal")
public class Decl: CustomStringConvertible, Hashable {
  public var bridged: BridgedDeclObj { BridgedDeclObj(SwiftObject(self)) }

  public var description: String { String(taking: bridged.getDebugDescription()) }

  public static func ==(lhs: Decl, rhs: Decl) -> Bool { lhs === rhs }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

public class ValueDecl: Decl {
  final public var nameLoc: SourceLoc? { SourceLoc(bridged: bridged.Value_getNameLoc()) }
  final public var userFacingName: StringRef { StringRef(bridged: bridged.Value_getUserFacingName()) }
  final public var isObjC: Bool { bridged.Value_isObjC() }
}

public class TypeDecl: ValueDecl {
  final public var name: StringRef { StringRef(bridged: bridged.Type_getName()) }
}

public class GenericTypeDecl: TypeDecl {
  final public var isGenericAtAnyLevel: Bool { bridged.GenericType_isGenericAtAnyLevel() }
}

public class NominalTypeDecl: GenericTypeDecl {
  final public var isGlobalActor: Bool { bridged.NominalType_isGlobalActor() }

  final public var valueTypeDestructor: DestructorDecl? {
    bridged.NominalType_getValueTypeDestructor().getAs(DestructorDecl.self)
  }
}

final public class EnumDecl: NominalTypeDecl {}

final public class StructDecl: NominalTypeDecl {
  public var hasUnreferenceableStorage: Bool { bridged.Struct_hasUnreferenceableStorage() }
}

final public class ClassDecl: NominalTypeDecl {
  public var superClass: Type? { Type(bridgedOrNil: bridged.Class_getSuperclass()) }

  final public var destructor: DestructorDecl {
    bridged.Class_getDestructor().getAs(DestructorDecl.self)
  }
}

final public class ProtocolDecl: NominalTypeDecl {}

final public class BuiltinTupleDecl: NominalTypeDecl {}

final public class OpaqueTypeDecl: GenericTypeDecl {}

final public class TypeAliasDecl: GenericTypeDecl {}

final public class GenericTypeParamDecl: TypeDecl {}

final public class AssociatedTypeDecl: TypeDecl {}

final public class ModuleDecl: TypeDecl {}

public class AbstractStorageDecl: ValueDecl {}

public class VarDecl: AbstractStorageDecl {}

final public class ParamDecl: VarDecl {}

final public class SubscriptDecl: AbstractStorageDecl {}

public class AbstractFunctionDecl: ValueDecl {}

final public class ConstructorDecl: AbstractFunctionDecl {}

final public class DestructorDecl: AbstractFunctionDecl {
  final public var isIsolated: Bool { bridged.Destructor_isIsolated() }
}

public class FuncDecl: AbstractFunctionDecl {}

final public class AccessorDecl: FuncDecl {}

final public class MacroDecl: ValueDecl {}

final public class EnumElementDecl: ValueDecl {}

final public class ExtensionDecl: Decl {}

final public class TopLevelCodeDecl: Decl {}

final public class ImportDecl: Decl {}

final public class PoundDiagnosticDecl: Decl {}

final public class PrecedenceGroupDecl: Decl {}

final public class MissingDecl: Decl {}

final public class MissingMemberDecl: Decl {}

final public class PatternBindingDecl: Decl {}

final public class EnumCaseDecl: Decl {}

public class OperatorDecl: Decl {}

final public class InfixOperatorDecl: OperatorDecl {}

final public class PrefixOperatorDecl: OperatorDecl {}

final public class PostfixOperatorDecl: OperatorDecl {}

final public class MacroExpansionDecl: Decl {}

// Bridging utilities

extension BridgedDeclObj {
  public var decl: Decl { obj.getAs(Decl.self) }
  public func getAs<T: Decl>(_ declType: T.Type) -> T { obj.getAs(T.self) }
}

extension OptionalBridgedDeclObj {
  public var decl: Decl? { obj.getAs(Decl.self) }
  public func getAs<T: Decl>(_ declType: T.Type) -> T? { obj.getAs(T.self) }
}
