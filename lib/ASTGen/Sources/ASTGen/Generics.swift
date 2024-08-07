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
  func generate(genericParameterClause node: GenericParameterClauseSyntax) -> BridgedGenericParamList {
    .createParsed(
      self.ctx,
      leftAngleLoc: self.generateSourceLoc(node.leftAngle),
      parameters: node.parameters.lazy.map(self.generate).bridgedArray(in: self),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      rightAngleLoc: self.generateSourceLoc(node.rightAngle)
    )
  }

  func generate(genericParameter node: GenericParameterSyntax) -> BridgedGenericTypeParamDecl {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

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

    var paramKind: BridgedGenericTypeParamKind = .type

    if node.specifier?.tokenKind == .keyword(.each) {
      paramKind = .pack
    } else if node.specifier?.tokenKind == .keyword(.let) {
      paramKind = .value
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      specifierLoc: self.generateSourceLoc(node.specifier),
      name: name,
      nameLoc: nameLoc,
      inheritedType: self.generate(type: node.inheritedType),
      index: genericParameterIndex,
      paramKind: paramKind
    )
  }

  func generate(genericWhereClause node: GenericWhereClauseSyntax) -> BridgedTrailingWhereClause {
    let requirements = node.requirements.lazy.map {
      switch $0.requirement {
      case .conformanceRequirement(let conformance):
        return BridgedRequirementRepr(
          SeparatorLoc: self.generateSourceLoc(conformance.colon),
          Kind: .typeConstraint,
          FirstType: self.generate(type: conformance.leftType),
          SecondType: self.generate(type: conformance.rightType)
        )
      case .sameTypeRequirement(let sameType):
        return BridgedRequirementRepr(
          SeparatorLoc: self.generateSourceLoc(sameType.equal),
          Kind: .sameType,
          FirstType: self.generate(type: sameType.leftType),
          SecondType: self.generate(type: sameType.rightType)
        )
      case .layoutRequirement(_):
        // FIXME: Implement layout requirement translation.
        fatalError("Translation of layout requirements not implemented!")
#if RESILIENT_SWIFT_SYNTAX
      @unknown default:
        fatalError()
#endif
      }
    }

    return BridgedTrailingWhereClause.createParsed(
      self.ctx,
      whereKeywordLoc: self.generateSourceLoc(node.whereKeyword),
      requirements: requirements.bridgedArray(in: self)
    )
  }
}
