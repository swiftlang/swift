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

import SILBridging

// TODO: move all declarations to an AST module, once we have it

public struct NominalTypeDecl : Equatable, Hashable {
  public let bridged: BridgedNominalTypeDecl

  public init(_bridged: BridgedNominalTypeDecl) {
    self.bridged = _bridged
  }

  public var name: StringRef { StringRef(bridged: bridged.getName()) }

  public static func ==(lhs: NominalTypeDecl, rhs: NominalTypeDecl) -> Bool {
    lhs.bridged.raw == rhs.bridged.raw
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(bridged.raw)
  }

  public var location: Location { Location(bridged: BridgedLocation.fromNominalTypeDecl(bridged)) }

  public func isResilient(in function: Function) -> Bool {
    function.bridged.isResilientNominalDecl(bridged)
  }

  public var isStructWithUnreferenceableStorage: Bool {
    bridged.isStructWithUnreferenceableStorage()
  }

  public var isGlobalActor: Bool {
    return bridged.isGlobalActor()
  }

  public var hasValueDeinit: Bool {
    return bridged.hasValueDeinit()
  }

  public var isClass: Bool { bridged.isClass() }

  public var superClassType: Type? {
    precondition(isClass)
    return BridgedType.getSuperClassTypeOfClassDecl(bridged).typeOrNil
  }

  public var isGenericAtAnyLevel: Bool { bridged.isGenericAtAnyLevel() }
}

public struct ProtocolDecl {
  let bridged: BridgedProtocolDecl

  public init(_bridged: BridgedProtocolDecl) {
    self.bridged = _bridged
  }
}

public struct AssociatedTypeDecl {
  let bridged: BridgedAssociatedTypeDecl

  public init(_bridged: BridgedAssociatedTypeDecl) {
    self.bridged = _bridged
  }
}

public struct DeclRef {
  public let bridged: BridgedDeclRef

  public var location: Location { Location(bridged: bridged.getLocation()) }
}
