import CASTBridging
import CBasicBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

extension ASTGenVisitor {
  func visit(_ node: GenericParameterClauseSyntax) -> ASTNode {
    .misc(
      GenericParamList_create(
        self.ctx,
        bridgedSourceLoc(for: node.leftAngle),
        node.parameters.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self),
        self.visit(node.genericWhereClause)?.rawValue,
        bridgedSourceLoc(for: node.rightAngle)
      )
    )
  }

  func visit(_ node: GenericParameterSyntax) -> ASTNode {
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
        self.ctx,
        self.declContext,
        self.bridgedSourceLoc(for: node.eachKeyword),
        name,
        nameLoc,
        self.visit(node.inheritedType)?.rawValue,
        SwiftInt(genericParameterIndex)
      )
    )
  }

  func visit(_ node: GenericWhereClauseSyntax) -> ASTNode {
    let requirements = node.requirements.lazy.map {
      switch $0.requirement {
      case .conformanceRequirement(let conformance):
        return BridgedRequirementRepr(
          SeparatorLoc: self.bridgedSourceLoc(for: conformance.colon),
          Kind: .typeConstraint,
          FirstType: self.visit(conformance.leftType).rawValue,
          SecondType: self.visit(conformance.rightType).rawValue
        )
      case .sameTypeRequirement(let sameType):
        return BridgedRequirementRepr(
          SeparatorLoc: self.bridgedSourceLoc(for: sameType.equal),
          Kind: .sameType,
          FirstType: self.visit(sameType.leftType).rawValue,
          SecondType: self.visit(sameType.rightType).rawValue
        )
      case .layoutRequirement(_):
        // FIXME: Implement layout requirement translation.
        fatalError("Translation of layout requirements not implemented!")
      }
    }

    return .misc(
      TrailingWhereClause_create(
        self.ctx,
        self.bridgedSourceLoc(for: node.whereKeyword),
        requirements.bridgedArray(in: self)
      )
    )
  }
}
