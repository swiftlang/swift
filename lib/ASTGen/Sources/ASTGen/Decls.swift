import CASTBridging
import SwiftParser
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: TypealiasDeclSyntax) -> ASTNode {
    let aliasLoc = self.base.advanced(by: node.typealiasKeyword.position.utf8Offset).raw
    let equalLoc = self.base.advanced(by: node.initializer.equal.position.utf8Offset).raw
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }
    let nameLoc = self.base.advanced(by: node.identifier.position.utf8Offset).raw
    let genericParams = node.genericParameterClause.map(self.visit).map { $0.rawValue }
    let out = TypeAliasDecl_create(
      self.ctx, self.declContext, aliasLoc, equalLoc, name, nameLoc, genericParams)

    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    let underlying = self.visit(node.initializer.value).rawValue
    TypeAliasDecl_setUnderlyingTypeRepr(out.nominalDecl, underlying)

    return .decl(out.decl)
  }

  public func visit(_ node: StructDeclSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    let genericParams = node.genericParameterClause
      .map(self.visit)
      .map { $0.rawValue }
    let out = StructDecl_create(ctx, loc, name, loc, genericParams, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.members.members.map(self.visit).withBridgedArrayRef { ref in
      NominalTypeDecl_setMembers(out.nominalDecl, ref)
    }

    return .decl(out.decl)
  }

  public func visit(_ node: ClassDeclSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    let out = ClassDecl_create(ctx, loc, name, loc, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.members.members.map(self.visit).withBridgedArrayRef { ref in
      NominalTypeDecl_setMembers(out.nominalDecl, ref)
    }

    return .decl(out.decl)
  }

  public func visit(_ node: VariableDeclSyntax) -> ASTNode {
    let pattern = visit(node.bindings.first!.pattern).rawValue
    let initializer = visit(node.bindings.first!.initializer!).rawValue

    let loc = self.base.advanced(by: node.position.utf8Offset).raw
    let isStateic = false  // TODO: compute this
    let isLet = node.letOrVarKeyword.tokenKind == .letKeyword

    // TODO: don't drop "initializer" on the floor.
    return .decl(SwiftVarDecl_create(ctx, pattern, loc, isStateic, isLet, declContext))
  }

  public func visit(_ node: FunctionParameterSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    let firstName: UnsafeMutableRawPointer?
    let secondName: UnsafeMutableRawPointer?
    let type: UnsafeMutableRawPointer?

    if let nodeFirstName = node.firstName {
      var text = nodeFirstName.text
      firstName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
    } else {
      firstName = nil
    }

    if let nodeSecondName = node.secondName {
      var text = nodeSecondName.text
      secondName = text.withUTF8 { buf in
        SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
    } else {
      secondName = nil
    }
    
    if let typeSyntax = node.type {
      type = visit(typeSyntax).rawValue
    } else {
      type = nil
    }

    return .decl(ParamDecl_create(ctx, loc, loc, firstName, loc, secondName, type, declContext))
  }

  public func visit(_ node: FunctionDeclSyntax) -> ASTNode {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    var nameText = node.identifier.text
    let name = nameText.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    let body: ASTNode?
    if let nodeBody = node.body {
      body = visit(nodeBody)
    } else {
      body = nil
    }

    let returnType: ASTNode?
    if let output = node.signature.output {
      returnType = visit(output.returnType)
    } else {
      returnType = nil
    }

    let params = node.signature.input.parameterList.map { visit($0) }
    return .decl(
      params.withBridgedArrayRef { ref in
        FuncDecl_create(
          ctx, loc, false, loc, name, loc, false, nil, false, nil, loc, ref, loc, body?.rawValue,
          returnType?.rawValue, declContext)
      })
  }
}
