//===--- Generics.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import SwiftSyntax

extension ASTGenVisitor {
  func generate(_ node: GenericParameterClauseSyntax) -> BridgedGenericParamList {
    .createParsed(
      self.ctx,
      leftAngleLoc: node.leftAngle.bridgedSourceLoc(in: self),
      parameters: node.parameters.lazy.map(self.generate).bridgedArray(in: self),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      rightAngleLoc: node.rightAngle.bridgedSourceLoc(in: self)
    )
  }

  func generate(_ node: GenericParameterSyntax) -> BridgedGenericTypeParamDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    var genericParameterIndex: Int?
    for (index, sibling) in (node.parent?.as(GenericParameterListSyntax.self) ?? []).enumerated() {
      if sibling == node {
        genericParameterIndex = index
        break
      }
    }
    guard let genericParameterIndex = genericParameterIndex else {
      preconditionFailure("Node not part of the parent?")
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      eachKeywordLoc: node.eachKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      inheritedType: self.generate(node.inheritedType).asNullable,
      index: genericParameterIndex
    )
  }

  func generate(_ node: GenericWhereClauseSyntax) -> BridgedTrailingWhereClause {
    let requirements = node.requirements.lazy.map {
      switch $0.requirement {
      case .conformanceRequirement(let conformance):
        return BridgedRequirementRepr(
          SeparatorLoc: conformance.colon.bridgedSourceLoc(in: self),
          Kind: .typeConstraint,
          FirstType: self.generate(conformance.leftType),
          SecondType: self.generate(conformance.rightType)
        )
      case .sameTypeRequirement(let sameType):
        return BridgedRequirementRepr(
          SeparatorLoc: sameType.equal.bridgedSourceLoc(in: self),
          Kind: .sameType,
          FirstType: self.generate(sameType.leftType),
          SecondType: self.generate(sameType.rightType)
        )
      case .layoutRequirement(_):
        // FIXME: Implement layout requirement translation.
        fatalError("Translation of layout requirements not implemented!")
      }
    }

    return BridgedTrailingWhereClause.createParsed(
      self.ctx,
      whereKeywordLoc: node.whereKeyword.bridgedSourceLoc(in: self),
      requirements: requirements.bridgedArray(in: self)
    )
  }
}
