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
/// It is not necessarily canoncial, e.g. typealiases are not resolved.
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

  public var type: Type { self }

  public init?(bridgedOrNil: BridgedASTType) {
    if bridgedOrNil.type == nil {
      return nil
    }
    self.bridged = bridgedOrNil
  }

  init(bridged: BridgedASTType) {
    self.bridged = bridged
  }

  public var canonical: CanonicalType { CanonicalType(bridged: bridged.getCanonicalType()) }

  public var instanceTypeOfMetatype: Type { Type(bridged: bridged.getInstanceTypeOfMetatype()) }

  public func subst(with substitutionMap: SubstitutionMap) -> Type {
    return Type(bridged: bridged.subst(substitutionMap.bridged))
  }

  public func subst(type: Type, with targetType: Type) -> Type {
    return Type(bridged: bridged.subst(type.bridged, targetType.bridged))
  }
}

/// A Type that is statically known to be canonical.
/// For example, typealiases are resolved.
public struct CanonicalType: TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedCanType

  public init(bridged: BridgedCanType) { self.bridged = bridged }

  public var type: Type { Type(bridged: bridged.getType()) }

  public var instanceTypeOfMetatype: CanonicalType { type.instanceTypeOfMetatype.canonical }
  
  public func subst(with substitutionMap: SubstitutionMap) -> CanonicalType {
    return type.subst(with: substitutionMap).canonical
  }

  public func subst(type: CanonicalType, with targetType: CanonicalType) -> CanonicalType {
    return self.type.subst(type: type.type, with: targetType.type).canonical
  }
}

/// Contains common properties of AST.Type and AST.CanonicalType
public protocol TypeProperties {
  var type: Type { get }
}

extension TypeProperties {
  public var description: String { String(taking: type.bridged.getDebugDescription()) }

  public var isLegalFormalType: Bool { type.bridged.isLegalFormalType() }
  public var hasTypeParameter: Bool { type.bridged.hasTypeParameter() }
  public var hasLocalArchetype: Bool { type.bridged.hasLocalArchetype() }
  public var isExistentialArchetype: Bool { type.bridged.isExistentialArchetype() }
  public var isExistentialArchetypeWithError: Bool { type.bridged.isExistentialArchetypeWithError() }
  public var isExistential: Bool { type.bridged.isExistential() }
  public var isEscapable: Bool { type.bridged.isEscapable() }
  public var isNoEscape: Bool { type.bridged.isNoEscape() }
  public var isInteger: Bool { type.bridged.isInteger() }
  public var isMetatypeType: Bool { type.bridged.isMetatypeType() }
  public var isExistentialMetatypeType: Bool { type.bridged.isExistentialMetatypeType() }
  public var representationOfMetatype: AST.`Type`.MetatypeRepresentation {
    type.bridged.getRepresentationOfMetatype().representation
  }
  public var invocationGenericSignatureOfFunctionType: GenericSignature {
    GenericSignature(bridged: type.bridged.getInvocationGenericSignatureOfFunctionType())
  }

  public var canBeClass: Type.TraitResult { type.bridged.canBeClass().result }

  public var anyNominal: NominalTypeDecl? { type.bridged.getAnyNominal().getAs(NominalTypeDecl.self) }

  /// Performas a global conformance lookup for this type for `protocol`.
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
    return Conformance(bridged: type.bridged.checkConformance(`protocol`.bridged))
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
    lhs.type == rhs.type
  }
}
