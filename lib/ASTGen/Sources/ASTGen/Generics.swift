import CASTBridging
import CBasicBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

extension ASTGenVisitor {
  func visit(_ node: GenericParameterClauseSyntax) -> ASTNode {
    let lAngleLoc = bridgedSourceLoc(for: node.leftAngle)
    let whereLoc = bridgedSourceLoc(for: node.genericWhereClause?.whereKeyword)
    let rAngleLoc = bridgedSourceLoc(for: node.rightAngle)
    return .misc(
      self.withBridgedParametersAndRequirements(node) { params, reqs in
        return GenericParamList_create(self.ctx, lAngleLoc, params, whereLoc, reqs, rAngleLoc)
      })
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
}

extension ASTGenVisitor {
  private func withBridgedParametersAndRequirements<T>(
    _ node: GenericParameterClauseSyntax,
    action: (BridgedArrayRef, BridgedArrayRef) -> T
  ) -> T {
    let parameters = node.parameters.lazy.map { self.visit($0).rawValue }

    let requirements = node.genericWhereClause?.requirements.lazy.map {
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
        fatalError("Cannot handle layout requirements!")
      }
    }

    return action(parameters.bridgedArray(in: self), requirements.bridgedArray(in: self))
  }
}
