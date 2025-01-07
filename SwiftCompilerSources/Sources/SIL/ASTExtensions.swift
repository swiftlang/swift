//===--- ASTExtensions.swift ----------------------------------------------===//
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

import AST
import SILBridging

extension AST.`Type` {
  // See `CanonicalType.loweredType(in:)`.
  public func loweredType(in function: Function, maximallyAbstracted: Bool = false) -> Type {
    function.bridged.getLoweredType(bridged, maximallyAbstracted).type.objectType
  }
}

extension CanonicalType {
  // This can yield nil if the AST type is not a lowered type.
  // For example, if the AST type is a `AnyFunctionType` for which the lowered type would be a `SILFunctionType`.
  public var silType: Type? {
    BridgedType.createSILType(bridged).typeOrNil
  }

  // Lowers the AST type to a SIL type - in a specific function.
  // In contrast to `silType` this always succeeds. Still, it's not allowed to do this for certain AST types
  // which are not present in SIL, like an `InOut` or LValue types.
  //
  // If `maximallyAbstracted` is true, the lowering is done with a completely opaque abstraction pattern
  // (see AbstractionPattern for details).
  public func loweredType(in function: Function, maximallyAbstracted: Bool = false) -> Type {
    type.loweredType(in: function, maximallyAbstracted: maximallyAbstracted)
  }
}

extension Decl {
  public var location: Location { Location(bridged: BridgedLocation.fromNominalTypeDecl(bridged)) }
}

extension NominalTypeDecl {
  public func isResilient(in function: Function) -> Bool {
    function.bridged.isResilientNominalDecl(bridged)
  }
}

extension ClassDecl {
  public var superClassType: Type? {
    self.superClass?.canonical.silType!
  }
}

extension SubstitutionMap {
  public func getMethodSubstitutions(for method: Function) -> SubstitutionMap {
    return SubstitutionMap(bridged: method.bridged.getMethodSubstitutions(bridged))
  }
}
