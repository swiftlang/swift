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

  public var canonicalSignature: CanonicalGenericSignature {
    CanonicalGenericSignature(bridged: bridged.getCanonicalSignature())
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
