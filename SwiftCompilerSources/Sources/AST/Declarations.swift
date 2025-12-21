//===--- Declarations.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
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
  final public var bridged: BridgedDeclObj { BridgedDeclObj(SwiftObject(self)) }

  final public var description: String { String(taking: bridged.getDebugDescription()) }

  /// The module in which this declaration resides.
  final public var parentModule: ModuleDecl { bridged.getModuleContext().getAs(ModuleDecl.self) }

  /// The parent DeclContext.
  final public var parentDeclContext: DeclContext? {
    if let decl = bridged.getParent().decl {
      return (decl as! DeclContext)
    }
    if let bridgedDeclContext = BridgedDeclContext(bridged: bridged.getDeclContext()) {
      // A DeclContext which is not a Decl.
      // TODO: once we have bridged those DeclContext classes, get rid of UnknownDeclContext
      return UnknownDeclContext(bridged: bridgedDeclContext)
    }
    return nil
  }

  // True if this declaration is imported from C/C++/ObjC.
  final public var hasClangNode: Bool { bridged.hasClangNode() }

  final public var bridgedDecl: BridgedDecl { BridgedDecl(raw: bridged.obj) }

  public static func ==(lhs: Decl, rhs: Decl) -> Bool { lhs === rhs }

  final public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  final public func setImplicit() { bridged.setImplicit() }
}

public protocol GenericContext: Decl, DeclContext {}

extension GenericContext {
  public func setGenericSignature(_ genericSignature: GenericSignature) {
    bridged.GenericContext_setGenericSignature(genericSignature.bridged)
  }

  public var bridgedDeclContext: BridgedDeclContext { bridged.asGenericContext() }
}

public class ValueDecl: Decl {
  final public var nameLoc: SourceLoc? { SourceLoc(bridged: bridged.Value_getNameLoc()) }
  final public var userFacingName: StringRef { StringRef(bridged: bridged.Value_getUserFacingName()) }
  final public var baseIdentifier: Identifier { bridged.Value_getBaseIdentifier() }
  final public var isObjC: Bool { bridged.Value_isObjC() }
  final public func setAccess(_ accessLevel : AccessLevel) {
    bridged.ValueDecl_setAccess(accessLevel)
  }
}

public class TypeDecl: ValueDecl {
  final public var name: StringRef { StringRef(bridged: bridged.Type_getName()) }
}

public class GenericTypeDecl: TypeDecl, GenericContext {
  final public var isGenericAtAnyLevel: Bool { bridged.GenericType_isGenericAtAnyLevel() }
}

public class NominalTypeDecl: GenericTypeDecl {
  final public var isGlobalActor: Bool { bridged.NominalType_isGlobalActor() }

  final public var valueTypeDestructor: DestructorDecl? {
    bridged.NominalType_getValueTypeDestructor().getAs(DestructorDecl.self)
  }

  public var declaredInterfaceType: Type {
    Type(bridged: bridged.NominalType_getDeclaredInterfaceType())
  }

  public func add(member: Decl) {
    bridged.NominalTypeDecl_addMember(member.bridged)
  }
}

final public class EnumDecl: NominalTypeDecl {
  public var hasRawType: Bool { bridged.Enum_hasRawType() }

  public static func create(
    declContext: DeclContext, enumKeywordLoc: SourceLoc?, name: String,
    nameLoc: SourceLoc?, genericParamList: GenericParameterList?, inheritedTypes: [Type],
    genericWhereClause: TrailingWhereClause?, braceRange: SourceRange, _ astContext: ASTContext
  ) -> EnumDecl {
    name.withCString { strPtr in
      inheritedTypes.withBridgedArrayRef { types in
        ASTBridging.BridgedEnumDecl.createParsed(
          astContext, declContext: declContext.bridgedDeclContext,
          enumKeywordLoc: enumKeywordLoc.bridgedLocation,
          name: astContext.getIdentifier(BridgedStringRef(data: strPtr, count: name.count)),
          nameLoc: nameLoc.bridgedLocation,
          genericParamList: genericParamList.bridged,
          inheritedTypes: types,
          genericWhereClause: genericWhereClause.bridged,
          braceRange: braceRange.bridged)
      }
    }.asDecl.declObj.getAs(EnumDecl.self)
  }
}

final public class StructDecl: NominalTypeDecl {
  public var hasUnreferenceableStorage: Bool { bridged.Struct_hasUnreferenceableStorage() }
}

final public class ClassDecl: NominalTypeDecl {
  public var superClass: Type? { Type(bridgedOrNil: bridged.Class_getSuperclass()) }

  final public var destructor: DestructorDecl {
    bridged.Class_getDestructor().getAs(DestructorDecl.self)
  }

  public var isForeign: Bool { bridged.Class_isForeign() }
}

final public class ProtocolDecl: NominalTypeDecl {
  public var requiresClass: Bool { bridged.ProtocolDecl_requiresClass() }
  public var isMarkerProtocol: Bool { bridged.ProtocolDecl_isMarkerProtocol() }
}

final public class BuiltinTupleDecl: NominalTypeDecl {}

final public class OpaqueTypeDecl: GenericTypeDecl {}

final public class TypeAliasDecl: GenericTypeDecl {}

final public class GenericTypeParamDecl: TypeDecl {
  public static func create(
    declContext: DeclContext,
    name: Identifier,
    depth: Int,
    index: Int,
    paramKind: GenericTypeParameterKind) -> GenericTypeParamDecl {
    ASTBridging.BridgedGenericTypeParamDecl.createImplicit(
      declContext: declContext.bridgedDeclContext,
      name: name, depth: depth, index: index,
      paramKind: paramKind).asDecl.declObj.getAs(GenericTypeParamDecl.self)
  }
}

final public class AssociatedTypeDecl: TypeDecl {}

final public class ModuleDecl: TypeDecl, DeclContext {
  public var bridgedDeclContext: BridgedDeclContext { bridged.asModuleDecl() }
}

public class AbstractStorageDecl: ValueDecl {
  final public var isConst: Bool { bridged.AbstractStorage_isConst() }
}

public class VarDecl: AbstractStorageDecl {}

final public class ParamDecl: VarDecl {
  public func cloneWithoutType() -> ParamDecl {
    BridgedParamDecl(raw: bridged.obj).cloneWithoutType().asDecl.declObj.getAs(ParamDecl.self)
  }

  public func setInterfaceType(type: Type) {
    BridgedParamDecl(raw: bridged.obj).setInterfaceType(type.bridged)
  }
}

final public class SubscriptDecl: AbstractStorageDecl, GenericContext {}

public class AbstractFunctionDecl: ValueDecl, GenericContext {
  final public var isOverridden: Bool { bridged.AbstractFunction_isOverridden() }
}

final public class ConstructorDecl: AbstractFunctionDecl {
  public var isInheritable: Bool { bridged.Constructor_isInheritable() }
}

final public class DestructorDecl: AbstractFunctionDecl {
  final public var isIsolated: Bool { bridged.Destructor_isIsolated() }
}

public class FuncDecl: AbstractFunctionDecl {}

final public class AccessorDecl: FuncDecl {}

final public class MacroDecl: ValueDecl {}

final public class EnumElementDecl: ValueDecl {
  public var hasAssociatedValues: Bool { bridged.EnumElementDecl_hasAssociatedValues() }
  public var parameterList: ParameterList { ParameterList(bridged: bridged.EnumElementDecl_getParameterList()) }
  public var name: StringRef { StringRef(bridged: bridged.EnumElementDecl_getNameStr()) }
  public var parentEnum: EnumDecl { self.parentDeclContext as! EnumDecl }

  public static func create(
    declContext: DeclContext,
    name: Identifier, nameLoc: SourceLoc?,
    parameterList: ParameterList?,
    equalsLoc: SourceLoc?, rawValue: Expr?, _ astContext: ASTContext
  ) -> EnumElementDecl {
    BridgedEnumElementDecl.createParsed(
      astContext, declContext: declContext.bridgedDeclContext,
      name: name, nameLoc: nameLoc.bridgedLocation,
      parameterList: parameterList.bridged.bridged,
      equalsLoc: equalsLoc.bridgedLocation,
      rawValue: rawValue.bridged).asDecl.declObj.getAs(EnumElementDecl.self)
  }
}

final public class ExtensionDecl: Decl, GenericContext {}

final public class TopLevelCodeDecl: Decl, DeclContext {
  public var bridgedDeclContext: BridgedDeclContext { bridged.asTopLevelCodeDecl() }
}

final public class ImportDecl: Decl {}

final public class UsingDecl: Decl {}

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

extension Optional where Wrapped == Decl {
  public var bridged: OptionalBridgedDeclObj {
    OptionalBridgedDeclObj(self?.bridged.obj)
  }
}

public typealias AccessLevel = swift.AccessLevel

public typealias Identifier = swift.Identifier

public typealias GenericTypeParamKind = swift.GenericTypeParamKind

public typealias ASTContext = BridgedASTContext

public typealias Expr = BridgedExpr

public typealias SourceFile = BridgedSourceFile

public typealias FileUnit = BridgedFileUnit

public class GenericParameterList {
  public var bridged: BridgedGenericParamList
  public init(bridged: BridgedGenericParamList) { self.bridged = bridged }
}

public typealias TrailingWhereClause = BridgedTrailingWhereClause

public class ParameterList : RandomAccessCollection {
  public var startIndex: Int { 0 }
  public var endIndex: Int { bridged.size }

  public var bridged: BridgedParameterList

  public init(bridged: BridgedParameterList) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> ParamDecl {
    return bridged.get(index).asDecl.declObj.getAs(ParamDecl.self)
  }

  public static func create(
    leftParenLoc: SourceLoc?, parameters: [ParamDecl],
    rightParenLoc: SourceLoc?, _ astContext: ASTContext
  ) -> ParameterList {
    ParameterList(bridged: parameters.map{BridgedParamDecl(raw: $0.bridged.obj)}.withBridgedArrayRef {
      BridgedParameterList.createParsed(
        astContext, leftParenLoc: leftParenLoc.bridgedLocation, parameters: $0,
        rightParenLoc: rightParenLoc.bridgedLocation)
    })
  }
}

extension GenericParameterList {
  public static func create(
    leftAngleLoc: SourceLoc?, parameters: [GenericTypeParamDecl],
    genericWhereClause: TrailingWhereClause?,
    rightAngleLoc: SourceLoc?, _ astContext: ASTContext
  ) -> GenericParameterList {
    let paramsNew = parameters.map{ ASTBridging.BridgedGenericTypeParamDecl(raw: $0.bridged.obj) }
    return paramsNew.withBridgedArrayRef {
      GenericParameterList(bridged: BridgedGenericParamList.createParsed(
        astContext, leftAngleLoc: leftAngleLoc.bridgedLocation, parameters: $0,
        genericWhereClause: genericWhereClause.bridged, rightAngleLoc: rightAngleLoc.bridgedLocation
      ))
    }
  }
}

extension BridgedDecl {
  public var declObj: BridgedDeclObj {
    BridgedDeclObj(self)
  }
}

extension BridgedDeclContext {
  public init?(bridged: BridgedNullableDeclContext) {
    guard let raw = bridged.raw else {
      return nil
    }
    self.init(raw: raw)
  }
}

extension SourceFile {
  public init?(bridged: BridgedNullableSourceFile) {
    guard let raw = bridged.raw else {
      return nil
    }
    self.init(raw: raw)
  }
}

extension FileUnit {
  public var asSourceFile: SourceFile? { SourceFile(bridged: self.castToSourceFile()) }
}

extension ParameterList? {
  public var bridged: BridgedParameterList? {
    return self?.bridged
  }
}

extension BridgedParameterList? {
  public var bridged: BridgedNullableParameterList {
    BridgedNullableParameterList(raw: self?.raw)
  }
}

extension GenericParameterList? {
  public var bridged: BridgedNullableGenericParamList {
    BridgedNullableGenericParamList(raw: self?.bridged.raw)
  }
}

extension Expr? {
  public var bridged: BridgedNullableExpr {
    BridgedNullableExpr(raw: self?.raw)
  }
}

extension TrailingWhereClause? {
  public var bridged: BridgedNullableTrailingWhereClause {
    BridgedNullableTrailingWhereClause(raw: self?.raw)
  }
}
