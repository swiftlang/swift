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
public struct Type: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedASTType

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
  public var description: String { String(taking: bridged.getDebugDescription()) }

  public var hasTypeParameter: Bool { bridged.hasTypeParameter() }
  public var hasOpenedExistential: Bool { bridged.hasOpenedExistential() }
  public var isOpenedExistentialWithError: Bool { bridged.isOpenedExistentialWithError() }
  public var isEscapable: Bool { bridged.isEscapable() }
  public var isNoEscape: Bool { bridged.isNoEscape() }
  public var isInteger: Bool { bridged.isInteger() }
  public var isMetatypeType: Bool { bridged.isMetatypeType() }
  public var isExistentialMetatypeType: Bool { bridged.isExistentialMetatypeType() }

  public var anyNominal: NominalTypeDecl? { bridged.getAnyNominal().getAs(NominalTypeDecl.self) }
  public var instanceTypeOfMetatype: Type { Type(bridged: bridged.getInstanceTypeOfMetatype()) }
  
  public func subst(with substitutionMap: SubstitutionMap) -> Type {
    return Type(bridged: bridged.subst(substitutionMap.bridged))
  }
  
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
    return Conformance(bridged: bridged.checkConformance(`protocol`.bridged))
  }
}

/// A Type that is statically known to be canonical.
/// For example, typealiases are resolved.
public struct CanonicalType: CustomStringConvertible, NoReflectionChildren {
  public enum TraitResult {
    case isNot
    case canBe
    case `is`
  }

  public let bridged: BridgedCanType

  public init(bridged: BridgedCanType) { self.bridged = bridged }

  public var type: Type { Type(bridged: bridged.getType()) }

  public var description: String { type.description }

  public var hasTypeParameter: Bool { type.hasTypeParameter }
  public var hasOpenedExistential: Bool { type.hasOpenedExistential }
  public var isOpenedExistentialWithError: Bool { type.isOpenedExistentialWithError }
  public var isEscapable: Bool { type.isEscapable }
  public var isNoEscape: Bool { type.isNoEscape }
  public var isInteger: Bool { type.isInteger }
  public var isMetatypeType: Bool { type.isMetatypeType }
  public var isExistentialMetatypeType: Bool { type.isExistentialMetatypeType }

  public var anyNominal: NominalTypeDecl? { type.anyNominal }
  public var instanceTypeOfMetatype: CanonicalType { type.instanceTypeOfMetatype.canonical }
  
  public func subst(with substitutionMap: SubstitutionMap) -> CanonicalType {
    return type.subst(with: substitutionMap).canonical
  }

  // See `type.checkConformance` 
  public func checkConformance(to proto: ProtocolDecl) -> Conformance {
    return type.checkConformance(to: proto)
  }
  
  public var canBeClass: TraitResult { bridged.canBeClass().result }
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

extension BridgedCanType.TraitResult {
  var result: CanonicalType.TraitResult {
    switch self {
    case .IsNot: return .isNot
    case .CanBe: return .canBe
    case .Is:    return .is
    default:
      fatalError("wrong type TraitResult enum case")
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
