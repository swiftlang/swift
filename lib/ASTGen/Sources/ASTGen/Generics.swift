import CASTBridging
import CBasicBridging
import SwiftSyntax

extension ASTGenVisitor {
  func generate(_ node: GenericParameterClauseSyntax) -> ASTNode {
    .misc(
      GenericParamList_create(
        astContext: self.ctx,
        leftAngleLoc: node.leftAngle.bridgedSourceLoc(in: self),
        parameters: node.parameters.lazy.map { self.generate($0).rawValue }.bridgedArray(in: self),
        genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
        rightAngleLoc: node.rightAngle.bridgedSourceLoc(in: self)
      )
    )
  }

  func generate(_ node: GenericParameterSyntax) -> ASTNode {
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

    return .decl(
      GenericTypeParamDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        eachKeywordLoc: node.eachKeyword.bridgedSourceLoc(in: self),
        name: name,
        nameLoc: nameLoc,
        inheritedType: self.generate(node.inheritedType)?.rawValue,
        index: SwiftInt(genericParameterIndex)
      )
    )
  }

  func generate(_ node: GenericWhereClauseSyntax) -> ASTNode {
    let requirements = node.requirements.lazy.map {
      switch $0.requirement {
      case .conformanceRequirement(let conformance):
        return BridgedRequirementRepr(
          SeparatorLoc: conformance.colon.bridgedSourceLoc(in: self),
          Kind: .typeConstraint,
          FirstType: self.generate(conformance.leftType).rawValue,
          SecondType: self.generate(conformance.rightType).rawValue
        )
      case .sameTypeRequirement(let sameType):
        return BridgedRequirementRepr(
          SeparatorLoc: sameType.equal.bridgedSourceLoc(in: self),
          Kind: .sameType,
          FirstType: self.generate(sameType.leftType).rawValue,
          SecondType: self.generate(sameType.rightType).rawValue
        )
      case .layoutRequirement(_):
        // FIXME: Implement layout requirement translation.
        fatalError("Translation of layout requirements not implemented!")
      }
    }

    return .misc(
      TrailingWhereClause_create(
        astContext: self.ctx,
        whereKeywordLoc: node.whereKeyword.bridgedSourceLoc(in: self),
        requirements: requirements.bridgedArray(in: self)
      )
    )
  }
}
