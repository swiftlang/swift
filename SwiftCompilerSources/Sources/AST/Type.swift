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
  public var isOpenedExistentialWithError: Bool { bridged.isOpenedExistentialWithError() }
  public var isEscapable: Bool { bridged.isEscapable() }
  public var isNoEscape: Bool { bridged.isNoEscape() }
  public var isInteger: Bool { bridged.isInteger() }

  public func subst(with substitutionMap: SubstitutionMap) -> Type {
    return Type(bridged: bridged.subst(substitutionMap.bridged))
  }
}

/// A Type that is statically known to be canonical.
/// For example, typealiases are resolved.
public struct CanonicalType: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedCanType

  public init(bridged: BridgedCanType) { self.bridged = bridged }

  public var type: Type { Type(bridged: bridged.getType()) }

  public var description: String { type.description }

  public var hasTypeParameter: Bool { type.hasTypeParameter }
  public var isOpenedExistentialWithError: Bool { type.isOpenedExistentialWithError }
  public var isEscapable: Bool { type.isEscapable }
  public var isNoEscape: Bool { type.isNoEscape }
  public var isInteger: Bool { type.isInteger }

  public func subst(with substitutionMap: SubstitutionMap) -> CanonicalType {
    return type.subst(with: substitutionMap).canonical
  }
}
