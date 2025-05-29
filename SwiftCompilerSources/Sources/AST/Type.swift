//===--- Type.swift -------------------------------------------------------===//
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

/// A Swift type.
/// It is not necessarily canonical, e.g. typealiases are not resolved.
public struct Type: TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public enum TraitResult {
    case isNot
    case canBe
    case `is`
  }

  public enum MetatypeRepresentation {
    case thin
    case thick
    case objC
  };

  public let bridged: BridgedASTType

  // Needed to conform to TypeProperties
  public var rawType: Type { self }

  public init?(bridgedOrNil: BridgedASTType) {
    if bridgedOrNil.type == nil {
      return nil
    }
    self.bridged = bridgedOrNil
  }

  public init(bridged: BridgedASTType) {
    self.bridged = bridged
  }

  public var canonical: CanonicalType { CanonicalType(bridged: bridged.getCanonicalType()) }

  public var instanceTypeOfMetatype: Type { Type(bridged: bridged.getInstanceTypeOfMetatype()) }

  public var staticTypeOfDynamicSelf: Type { Type(bridged: bridged.getStaticTypeOfDynamicSelf()) }

  public var superClassType: Type? {
    precondition(isClass)
    let bridgedSuperClassTy = bridged.getSuperClassType()
    if bridgedSuperClassTy.type != nil {
      return Type(bridged: bridgedSuperClassTy)
    }
    return nil
  }

  public var builtinVectorElementType: Type { Type(bridged: bridged.getBuiltinVectorElementType()) }

  public func subst(with substitutionMap: SubstitutionMap) -> Type {
    return Type(bridged: bridged.subst(substitutionMap.bridged))
  }
}

/// A Type that is statically known to be canonical.
/// For example, typealiases are resolved.
public struct CanonicalType: TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedCanType

  public init(bridged: BridgedCanType) { self.bridged = bridged }

  public var rawType: Type { Type(bridged: bridged.getRawType()) }

  public var instanceTypeOfMetatype: CanonicalType { rawType.instanceTypeOfMetatype.canonical }

  public var superClassType: CanonicalType? { rawType.superClassType?.canonical }

  public var builtinVectorElementType: CanonicalType { rawType.builtinVectorElementType.canonical }

  public func subst(with substitutionMap: SubstitutionMap) -> CanonicalType {
    return rawType.subst(with: substitutionMap).canonical
  }
}

/// Implements the common members of `AST.Type`, `AST.CanonicalType` and `SIL.Type`.
public protocol TypeProperties {
  var rawType: Type { get }
}

extension TypeProperties {
  public var description: String { String(taking: rawType.bridged.getDebugDescription()) }

  //===--------------------------------------------------------------------===//
  //                      Checks for different kinds of types
  //===--------------------------------------------------------------------===//

  public var isBuiltinInteger: Bool { rawType.bridged.isBuiltinInteger() }

  public func isBuiltinInteger(withFixedWidth width: Int) -> Bool {
    rawType.bridged.isBuiltinFixedWidthInteger(width)
  }

  public var isBuiltinFloat: Bool { rawType.bridged.isBuiltinFloat() }
  public var isBuiltinVector: Bool { rawType.bridged.isBuiltinVector() }
  public var isBuiltinFixedArray: Bool { rawType.bridged.isBuiltinFixedArray() }

  public var isClass: Bool {
    if let nominal = nominal, nominal is ClassDecl {
      return true
    }
    return false
  }

  public var isStruct: Bool {
    if let nominal = nominal, nominal is StructDecl {
      return true
    }
    return false
  }

  public var isEnum: Bool  {
    if let nominal = nominal, nominal is EnumDecl {
      return true
    }
    return false
  }

  public var isTuple: Bool { rawType.bridged.isTuple() }
  public var isFunction: Bool { rawType.bridged.isFunction() }
  public var isArchetype: Bool { rawType.bridged.isArchetype() }
  public var isExistentialArchetype: Bool { rawType.bridged.isExistentialArchetype() }
  public var isExistentialArchetypeWithError: Bool { rawType.bridged.isExistentialArchetypeWithError() }
  public var isExistential: Bool { rawType.bridged.isExistential() }
  public var isClassExistential: Bool { rawType.bridged.isClassExistential() }
  public var isGenericTypeParameter: Bool { rawType.bridged.isGenericTypeParam() }
  public var isUnownedStorageType: Bool { return rawType.bridged.isUnownedStorageType() }
  public var isMetatype: Bool { rawType.bridged.isMetatypeType() }
  public var isExistentialMetatype: Bool { rawType.bridged.isExistentialMetatypeType() }
  public var isDynamicSelf: Bool { rawType.bridged.isDynamicSelf()}

  /// True if this is the type which represents an integer literal used in a type position.
  /// For example `N` in `struct T<let N: Int> {}`
  public var isInteger: Bool { rawType.bridged.isInteger() }

  public var canBeClass: Type.TraitResult { rawType.bridged.canBeClass().result }

  /// True if this the nominal type `Swift.Optional`.
  public var isOptional: Bool { rawType.bridged.isOptional() }

  /// True if this type is a value type (struct/enum) that defines a `deinit`.
  public var isValueTypeWithDeinit: Bool {
    if let nominal = nominal, nominal.valueTypeDestructor != nil {
      return true
    }
    return false
  }

  //===--------------------------------------------------------------------===//
  //                Properties of lowered `SILFunctionType`s
  //===--------------------------------------------------------------------===//

  public var isLoweredFunction: Bool { rawType.bridged.isLoweredFunction() }
  public var isNoEscapeFunction: Bool { rawType.bridged.isNoEscapeFunction() }
  public var isCalleeConsumedFunction: Bool { rawType.bridged.isCalleeConsumedFunction() }
  public var isThickFunction: Bool { rawType.bridged.isThickFunction() }
  public var isAsyncFunction: Bool { rawType.bridged.isAsyncFunction() }

  public var invocationGenericSignatureOfFunction: GenericSignature {
    GenericSignature(bridged: rawType.bridged.getInvocationGenericSignatureOfFunctionType())
  }

  //===--------------------------------------------------------------------===//
  //                             Type properties
  //===--------------------------------------------------------------------===//

  public var isLegalFormalType: Bool { rawType.bridged.isLegalFormalType() }
  public var hasArchetype: Bool { rawType.bridged.hasArchetype() }
  public var hasTypeParameter: Bool { rawType.bridged.hasTypeParameter() }
  public var hasLocalArchetype: Bool { rawType.bridged.hasLocalArchetype() }
  public var hasDynamicSelf: Bool { rawType.bridged.hasDynamicSelf() }
  public var isEscapable: Bool { rawType.bridged.isEscapable() }
  public var isNoEscape: Bool { rawType.bridged.isNoEscape() }
  public var isBuiltinType: Bool { rawType.bridged.isBuiltinType() }
  public var archetypeRequiresClass: Bool { rawType.bridged.archetypeRequiresClass() }

  public var representationOfMetatype: AST.`Type`.MetatypeRepresentation {
    rawType.bridged.getRepresentationOfMetatype().representation
  }

  public var builtinFixedArrayElementType: CanonicalType {
    CanonicalType(bridged: rawType.bridged.getBuiltinFixedArrayElementType())
  }
  public var builtinFixedArraySizeType: CanonicalType {
    CanonicalType(bridged: rawType.bridged.getBuiltinFixedArraySizeType())
  }

  /// Returns the value of an integer value type (see `isInteger`).
  /// Returns nil if the value is not representable in an `Int`.
  public var valueOfInteger: Int? {
    let optionalInt = rawType.bridged.getValueOfIntegerType()
    if optionalInt.hasValue {
      return optionalInt.value
    }
    return nil
  }

  /// Assumes this is a nominal type. Returns a substitution map that sends each
  /// generic parameter of the declaration's generic signature to the corresponding
  /// generic argument of this nominal type.
  ///
  /// Eg: Array<Int> ---> { Element := Int }
  public var contextSubstitutionMap: SubstitutionMap {
    SubstitutionMap(bridged: rawType.bridged.getContextSubstitutionMap())
  }

  // True if this type has generic parameters or it is in a context (e.g. an outer type) which has generic parameters.
  public var isGenericAtAnyLevel: Bool { rawType.bridged.isGenericAtAnyLevel() }

  public var nominal: NominalTypeDecl? {
    rawType.bridged.getNominalOrBoundGenericNominal().getAs(NominalTypeDecl.self)
  }

  /// Performs a global conformance lookup for this type for `protocol`.
  /// It checks conditional requirements.
  ///
  /// This type must be a contextualized type. It must not contain type parameters.
  ///
  /// The resulting conformance reference does not include "missing" conformances, which are synthesized for
  /// some protocols as an error recovery mechanism.
  ///
  /// Returns an invalid conformance if the search failed, otherwise an
  /// abstract, concrete or pack conformance, depending on the lookup type.
  public func checkConformance(to protocol: ProtocolDecl) -> Conformance {
    return Conformance(bridged: rawType.bridged.checkConformance(`protocol`.bridged))
  }
}

public struct TypeArray : RandomAccessCollection, CustomReflectable {
  public let bridged: BridgedASTTypeArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedASTTypeArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> Type {
    Type(bridged: bridged.getAt(index))
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}

extension BridgedASTType.TraitResult {
  var result: Type.TraitResult {
    switch self {
    case .IsNot: return .isNot
    case .CanBe: return .canBe
    case .Is:    return .is
    default:
      fatalError("wrong type TraitResult enum case")
    }
  }
}

extension BridgedASTType.MetatypeRepresentation {
  var representation: Type.MetatypeRepresentation {
    switch self {
    case .Thin:  return .thin
    case .Thick: return .thick
    case .ObjC:  return .objC
    default:
      fatalError("wrong type MetatypeRepresentation enum case")
    }
  }
}

extension Type: Equatable {
  public static func ==(lhs: Type, rhs: Type) -> Bool { 
    lhs.bridged.type == rhs.bridged.type
  }
}

extension CanonicalType: Equatable {
  public static func ==(lhs: CanonicalType, rhs: CanonicalType) -> Bool { 
    lhs.rawType == rhs.rawType
  }
}
