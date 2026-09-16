//===--- GenericSignature.swift -------------------------------------------===//
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

/// Describes the generic signature of a particular declaration, including both the generic type
/// parameters and the requirements placed on those generic parameters.
public struct GenericSignature: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedGenericSignature

  public init(bridged: BridgedGenericSignature) {
    self.bridged = bridged
  }
  
  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  public var genericParameters: TypeArray {
    TypeArray(bridged: bridged.getGenericParams())
  }

  public func mapTypeIntoEnvironment(_ type: Type) -> Type {
    Type(bridged: bridged.mapTypeIntoEnvironment(type.bridged))
  }

  public var isEmpty: Bool { bridged.impl == nil }

  /// Whether every generic parameter in this signature is constrained to a
  /// concrete type.
  public var areAllParamsConcrete: Bool { bridged.areAllParamsConcrete() }

  /// Whether every generic parameter in this signature is either constrained
  /// to a concrete type or class-constrained (the two forms permitted by
  /// Embedded Swift).
  public var canBeEmittedInEmbeddedSwift: Bool { bridged.canBeEmittedInEmbeddedSwift() }

  public var canonicalSignature: CanonicalGenericSignature {
    CanonicalGenericSignature(bridged: bridged.getCanonicalSignature())
  }
}

extension GenericSignature: Equatable {
  public static func == (lhs: GenericSignature, rhs: GenericSignature) -> Bool {
    lhs.bridged.impl == rhs.bridged.impl
  }
}

public struct CanonicalGenericSignature {
  public let bridged: BridgedCanGenericSignature

  public init(bridged: BridgedCanGenericSignature) {
    self.bridged = bridged
  }

  public var isEmpty: Bool { bridged.impl == nil }

  public var genericSignature: GenericSignature {
    GenericSignature(bridged: bridged.getGenericSignature())
  }
}

/// The generic environment that a local (opened existential) archetype lives in.
public struct GenericEnvironment {
  public let bridged: BridgedGenericEnvironment

  public init(bridged: BridgedGenericEnvironment) {
    self.bridged = bridged
  }

  public var genericSignature: GenericSignature {
    GenericSignature(bridged: bridged.getGenericSignature())
  }

  /// True if `self` and `other` have identical local-archetype requirements
  /// (conformances, superclass, layout constraint) and can therefore be
  /// safely remapped onto one another, e.g. by `Cloner`.
  public func hasEqualGenericSignature(to other: GenericEnvironment) -> Bool {
    genericSignature == other.genericSignature
  }
}
