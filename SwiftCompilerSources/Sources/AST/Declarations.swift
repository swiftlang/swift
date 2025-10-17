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

  /// The module in which this declaration resides.
  public var parentModule: ModuleDecl { bridged.getModuleContext().getAs(ModuleDecl.self) }

  /// The parent DeclContext if it is a Decl.
  public var parent: Decl? { bridged.getParent().decl }

  // True if this declaration is imported from C/C++/ObjC.
  public var hasClangNode: Bool { bridged.hasClangNode() }

  public var declContext: DeclContext { bridgedDecl.declContext }

  var bridgedDecl: BridgedDecl { BridgedDecl(raw: bridged.obj) }

  public static func ==(lhs: Decl, rhs: Decl) -> Bool { lhs === rhs }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

public class ValueDecl: Decl {
  final public var nameLoc: SourceLoc? { SourceLoc(bridged: bridged.Value_getNameLoc()) }
  final public var userFacingName: StringRef { StringRef(bridged: bridged.Value_getUserFacingName()) }
  final public var baseIdentifier: Identifier { bridged.Value_getBaseIdentifier() }
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

  public var declaredInterfaceType: Type {
    Type(bridged: bridged.NominalType_getDeclaredInterfaceType())
  }
}

final public class EnumDecl: NominalTypeDecl {
  public var hasRawType: Bool { bridged.Enum_hasRawType() }
}

final public class StructDecl: NominalTypeDecl {
  public var hasUnreferenceableStorage: Bool { bridged.Struct_hasUnreferenceableStorage() }
}

final public class ClassDecl: NominalTypeDecl {
  public var superClass: Type? { Type(bridgedOrNil: bridged.Class_getSuperclass()) }

  final public var destructor: DestructorDecl {
    bridged.Class_getDestructor().getAs(DestructorDecl.self)
  }
}

final public class ProtocolDecl: NominalTypeDecl {
  public var requiresClass: Bool { bridged.ProtocolDecl_requiresClass() }
}

final public class BuiltinTupleDecl: NominalTypeDecl {}

final public class OpaqueTypeDecl: GenericTypeDecl {}

final public class TypeAliasDecl: GenericTypeDecl {}

final public class GenericTypeParamDecl: TypeDecl {}

final public class AssociatedTypeDecl: TypeDecl {}

final public class ModuleDecl: TypeDecl {}

public class AbstractStorageDecl: ValueDecl {
  final public var isConst: Bool { bridged.AbstractStorage_isConst() }
}

public class VarDecl: AbstractStorageDecl {}

final public class ParamDecl: VarDecl {}

final public class SubscriptDecl: AbstractStorageDecl {}

public class AbstractFunctionDecl: ValueDecl {
  public var isOverridden: Bool { bridged.AbstractFunction_isOverridden() }
}

final public class ConstructorDecl: AbstractFunctionDecl {}

final public class DestructorDecl: AbstractFunctionDecl {
  final public var isIsolated: Bool { bridged.Destructor_isIsolated() }
}

public class FuncDecl: AbstractFunctionDecl {}

final public class AccessorDecl: FuncDecl {}

final public class MacroDecl: ValueDecl {}

final public class EnumElementDecl: ValueDecl {
  public var hasAssociatedValues: Bool { bridged.EnumElementDecl_hasAssociatedValues() }
  public var parameterList: ParameterList { bridged.EnumElementDecl_getParameterList() }
  public var name: StringRef { StringRef(bridged: bridged.EnumElementDecl_getNameStr()) }
}

final public class ExtensionDecl: Decl {}

final public class TopLevelCodeDecl: Decl {}

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

public typealias Identifier = swift.Identifier

public typealias GenericTypeParamKind = swift.GenericTypeParamKind

public typealias ASTContext = BridgedASTContext

public typealias DeclContext = BridgedDeclContext

public typealias Expr = BridgedExpr

public typealias ParameterList = BridgedParameterList

public typealias SourceFile = BridgedSourceFile

public typealias FileUnit = BridgedFileUnit

public typealias GenericParamList = BridgedGenericParamList

public typealias TrailingWhereClause = BridgedTrailingWhereClause

public typealias BridgedParamDecl = ASTBridging.BridgedParamDecl

public typealias BridgedGenericTypeParamDecl = ASTBridging.BridgedGenericTypeParamDecl

public typealias BridgedEnumDecl = ASTBridging.BridgedEnumDecl

public typealias BridgedEnumElementDecl = ASTBridging.BridgedEnumElementDecl

extension ParameterList {
  public subscript(_ index: Int) -> BridgedParamDecl {
    return get(index)
  }

  public static func createParsed(
    _ astContext: ASTContext, leftParenLoc: SourceLoc?, parameters: [BridgedParamDecl],
    rightParenLoc: SourceLoc?
  ) -> ParameterList {
    parameters.withBridgedArrayRef {
      ParameterList.createParsed(
        astContext, leftParenLoc: leftParenLoc.bridgedLocation, parameters: $0,
        rightParenLoc: rightParenLoc.bridgedLocation)
    }
  }
}

extension GenericParamList {
  public static func createParsed(
    _ astContext: ASTContext, leftAngleLoc: SourceLoc?, parameters: [BridgedGenericTypeParamDecl],
    genericWhereClause: TrailingWhereClause?,
    rightAngleLoc: SourceLoc?
  ) -> GenericParamList {
    return parameters.withBridgedArrayRef {
      GenericParamList.createParsed(
        astContext, leftAngleLoc: leftAngleLoc.bridgedLocation, parameters: $0,
        genericWhereClause: genericWhereClause.bridged, rightAngleLoc: rightAngleLoc.bridgedLocation
      )
    }
  }
}

extension BridgedDecl {
  public var declObj: BridgedDeclObj {
    BridgedDeclObj(SwiftObject(raw.bindMemory(to: BridgedSwiftObject.self, capacity: 1)))
  }
  public var decl: Decl { declObj.decl }
}

extension BridgedEnumDecl {
  public static func createParsed(
    _ astContext: ASTContext, declContext: DeclContext, enumKeywordLoc: SourceLoc?, name: String,
    nameLoc: SourceLoc?, genericParamList: GenericParamList?, inheritedTypes: [Type],
    genericWhereClause: TrailingWhereClause?, braceRange: SourceRange
  ) -> BridgedEnumDecl {
    return name.withCString { strPtr in
      inheritedTypes.withBridgedArrayRef { types in
        BridgedEnumDecl.createParsed(
          astContext, declContext: declContext,
          enumKeywordLoc: enumKeywordLoc.bridgedLocation,
          name: astContext.getIdentifier(BridgedStringRef(data: strPtr, count: name.count)),
          nameLoc: nameLoc.bridgedLocation,
          genericParamList: genericParamList.bridged,
          inheritedTypes: types,
          genericWhereClause: genericWhereClause.bridged,
          braceRange: braceRange.bridged)
      }
    }
  }
}

extension BridgedEnumElementDecl {
  public static func createParsed(
    _ astContext: ASTContext, declContext: DeclContext,
    name: Identifier, nameLoc: SourceLoc?,
    parameterList: ParameterList?,
    equalsLoc: SourceLoc?, rawValue: Expr?
  ) -> BridgedEnumElementDecl {
    BridgedEnumElementDecl.createParsed(
      astContext, declContext: declContext,
      name: name, nameLoc: nameLoc.bridgedLocation,
      parameterList: parameterList.bridged,
      equalsLoc: equalsLoc.bridgedLocation, rawValue: rawValue.bridged)
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

extension BridgedParamDecl {
  public func setInterfaceType(type: Type) {
    self.setInterfaceType(type.bridged)
  }
}

extension ParameterList? {
  public var bridged: BridgedNullableParameterList {
    BridgedNullableParameterList(raw: self?.raw)
  }
}

extension GenericParamList? {
  public var bridged: BridgedNullableGenericParamList {
    BridgedNullableGenericParamList(raw: self?.raw)
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
