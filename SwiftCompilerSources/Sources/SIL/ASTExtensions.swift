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

extension CanonicalType {
  var objectType: Type { BridgedType.createObjectType(bridged).type }
  var addressType: Type { BridgedType.createAddressType(bridged).type }
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
    self.superClass?.canonical.objectType
  }
}

extension SubstitutionMap {
  public func getMethodSubstitutions(for method: Function) -> SubstitutionMap {
    return SubstitutionMap(bridged: method.bridged.getMethodSubstitutions(bridged))
  }

  public var replacementTypes: OptionalTypeArray {
    let types = BridgedTypeArray.fromReplacementTypes(bridged)
    return OptionalTypeArray(bridged: types)
  }
}
