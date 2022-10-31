import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  func visit(_ node: GenericParameterClauseSyntax) -> ASTNode {
    let lAngleLoc = self.base.advanced(by: node.leftAngleBracket.position.utf8Offset).raw
    let whereLoc = node.genericWhereClause.map {
      self.base.advanced(by: $0.whereKeyword.position.utf8Offset).raw
    }
    let rAngleLoc = self.base.advanced(by: node.rightAngleBracket.position.utf8Offset).raw
    return .misc(
      self.withBridgedParametersAndRequirements(node) { params, reqs in
        return GenericParamList_create(self.ctx, lAngleLoc, params, whereLoc, reqs, rAngleLoc)
      })
  }

  func visit(_ node: GenericParameterSyntax) -> ASTNode {
    var nodeName = node.name.text
    let name = nodeName.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    let nameLoc = self.base.advanced(by: node.name.position.utf8Offset).raw
    let ellipsisLoc = node.ellipsis.map { self.base.advanced(by: $0.position.utf8Offset).raw }

    return .decl(
      GenericTypeParamDecl_create(
        self.ctx, self.declContext, name, nameLoc, ellipsisLoc, node.indexInParent / 2,
        ellipsisLoc != nil))
  }
}

extension ASTGenVisitor {
  private func withBridgedParametersAndRequirements<T>(
    _ node: GenericParameterClauseSyntax,
    action: (BridgedArrayRef, BridgedArrayRef) -> T
  ) -> T {
    var params = [UnsafeMutableRawPointer]()
    var requirements = [BridgedRequirementRepr]()
    for param in node.genericParameterList {
      let loweredParameter = self.visit(param).rawValue
      params.append(loweredParameter)

      guard let requirement = param.inheritedType else {
        continue
      }

      let loweredRequirement = self.visit(requirement)
      GenericTypeParamDecl_setInheritedType(self.ctx, loweredParameter, loweredRequirement.rawValue)
    }

    if let nodeRequirements = node.genericWhereClause?.requirementList {
      for requirement in nodeRequirements {
        switch requirement.body {
        case .conformanceRequirement(let conformance):
          let firstType = self.visit(conformance.leftTypeIdentifier).rawValue
          let separatorLoc = self.base.advanced(by: conformance.colon.position.utf8Offset).raw
          let secondType = self.visit(conformance.rightTypeIdentifier).rawValue
          requirements.append(
            BridgedRequirementRepr(
              SeparatorLoc: separatorLoc,
              Kind: .typeConstraint,
              FirstType: firstType,
              SecondType: secondType))
        case .sameTypeRequirement(let sameType):
          let firstType = self.visit(sameType.leftTypeIdentifier).rawValue
          let separatorLoc = self.base.advanced(by: sameType.equalityToken.position.utf8Offset).raw
          let secondType = self.visit(sameType.rightTypeIdentifier).rawValue
          requirements.append(
            BridgedRequirementRepr(
              SeparatorLoc: separatorLoc,
              Kind: .sameType,
              FirstType: firstType,
              SecondType: secondType))
        case .layoutRequirement(_):
          fatalError("Cannot handle layout requirements!")
        }
      }
    }
    return params.withBridgedArrayRef { params in
      return requirements.withBridgedArrayRef { reqs in
        return action(params, reqs)
      }
    }
  }
}
