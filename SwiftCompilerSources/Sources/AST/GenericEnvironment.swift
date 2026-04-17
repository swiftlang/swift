//===--- GenericEnvironment.swift -----------------------------------------===//
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

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
public struct GenericEnvironment: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedGenericEnvironment

  public init(bridged: BridgedGenericEnvironment) {
    self.bridged = bridged
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  /// The opened pack parameters for this opened-element environment,
  /// parallel to the element parameters of the innermost generic context.
  public var openedPackParams: TypeArray {
    TypeArray(bridged: bridged.getOpenedPackParams())
  }

  /// Map an interface type to a contextual type.
  public func mapTypeIntoEnvironment(_ type: Type) -> Type {
    Type(bridged: bridged.mapTypeIntoEnvironment(type.bridged))
  }

  /// Map a type containing pack element type parameters to a contextual
  /// type in the pack generic context.
  public func mapElementTypeIntoPackContext(_ type: Type) -> Type {
    Type(bridged: bridged.mapElementTypeIntoPackContext(type.bridged))
  }
}
