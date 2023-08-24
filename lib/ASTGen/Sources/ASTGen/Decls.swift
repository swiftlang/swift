import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: TypeAliasDeclSyntax) -> ASTNode {
    let aliasLoc = bridgedSourceLoc(for: node.typealiasKeyword)
    let equalLoc = bridgedSourceLoc(for: node.initializer.equal)
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let out = TypeAliasDecl_create(
      self.ctx, self.declContext, aliasLoc, equalLoc, name, nameLoc, self.visit(node.genericParameterClause)?.rawValue)

    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    let underlying = self.visit(node.initializer.value).rawValue
    TypeAliasDecl_setUnderlyingTypeRepr(out.nominalDecl, underlying)

    return .decl(out.decl)
  }

  public func visit(_ node: StructDeclSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let name = node.name.bridgedIdentifier(in: self)

    let out = StructDecl_create(ctx, loc, name, loc, self.visit(node.genericParameterClause)?.rawValue, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.memberBlock.members
      .map { self.visit($0).rawValue }
      .withBridgedArrayRef { ref in
        NominalTypeDecl_setMembers(out.nominalDecl, ref)
      }

    return .decl(out.decl)
  }

  public func visit(_ node: ClassDeclSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)
    let name = node.name.bridgedIdentifier(in: self)

    let out = ClassDecl_create(ctx, loc, name, loc, declContext)
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    node.memberBlock.members
      .map { self.visit($0).rawValue }
      .withBridgedArrayRef { ref in
        NominalTypeDecl_setMembers(out.nominalDecl, ref)
      }

    return .decl(out.decl)
  }

  public func visit(_ node: VariableDeclSyntax) -> ASTNode {
    let pattern = visit(node.bindings.first!.pattern).rawValue
    let initializer = visit(node.bindings.first!.initializer!).rawValue

    let loc = bridgedSourceLoc(for: node)
    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.tokenKind == .keyword(.let)

    // TODO: don't drop "initializer" on the floor.
    return .decl(
      VarDecl_create(
        ctx, pattern, initializer, loc, isStatic,
        isLet, declContext))
  }

  public func visit(_ node: FunctionParameterSyntax) -> ASTNode {
    let loc = bridgedSourceLoc(for: node)

    let firstName: BridgedIdentifier
    if node.firstName.text != "_" {
      // Swift AST represents "_" as nil.
      firstName = node.firstName.bridgedIdentifier(in: self)
    } else {
      firstName = nil
    }

    let secondName = node.secondName.bridgedIdentifier(in: self)

    let type = visit(node.type).rawValue

    return .decl(ParamDecl_create(ctx, loc, loc, firstName, loc, secondName, type, declContext))
  }

  public func visit(_ node: FunctionDeclSyntax) -> ASTNode {
    let staticLoc = bridgedSourceLoc(for: node)
    let funcLoc = bridgedSourceLoc(for: node.funcKeyword)
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let rParamLoc = bridgedSourceLoc(for: node.signature.parameterClause.leftParen)
    let lParamLoc = bridgedSourceLoc(for: node.signature.parameterClause.rightParen)

    let returnType: ASTNode?
    if let output = node.signature.returnClause {
      returnType = visit(output.type)
    } else {
      returnType = nil
    }

    let params = node.signature.parameterClause.parameters.map { visit($0).rawValue }
    let out = params.withBridgedArrayRef { ref in
      FuncDecl_create(
        ctx, staticLoc, false, funcLoc, name, nameLoc, false, nil, false, nil, rParamLoc, ref,
        lParamLoc,
        returnType?.rawValue, declContext)
    }

    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    let body: ASTNode?
    if let nodeBody = node.body {
      body = visit(nodeBody)
    } else {
      body = nil
    }

    if let body = body {
      FuncDecl_setBody(out.funcDecl, body.rawValue)
    }

    return .decl(out.decl)
  }
}
