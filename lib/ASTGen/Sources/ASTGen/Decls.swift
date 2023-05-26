import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

extension ASTGenVisitor {
  public func visit(_ node: TypeAliasDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      TypeAliasDecl_create(
        self.ctx,
        self.declContext,
        self.bridgedSourceLoc(for: node.typealiasKeyword),
        name,
        nameLoc,
        self.visit(node.genericParameterClause)?.rawValue,
        self.bridgedSourceLoc(for: node.initializer.equal),
        self.visit(node.initializer.value).rawValue
      )
    )
  }

  public func visit(_ node: StructDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let out = StructDecl_create(
      self.ctx,
      self.declContext,
      self.bridgedSourceLoc(for: node.structKeyword),
      name,
      nameLoc,
      self.visit(node.genericParameterClause)?.rawValue
    )

    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    NominalTypeDecl_setMembers(out.nominalDecl, self.visit(node.memberBlock.members))

    return .decl(out.decl)
  }

  public func visit(_ node: ClassDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let out = ClassDecl_create(
      self.ctx,
      self.declContext,
      self.bridgedSourceLoc(for: node.classKeyword),
      name,
      nameLoc
    )
    let oldDeclContext = declContext
    declContext = out.declContext
    defer { declContext = oldDeclContext }

    NominalTypeDecl_setMembers(out.nominalDecl, self.visit(node.memberBlock.members))

    return .decl(out.decl)
  }

  public func visit(_ node: VariableDeclSyntax) -> ASTNode {
    let pattern = visit(node.bindings.first!.pattern).rawValue
    let initializer = visit(node.bindings.first!.initializer!).rawValue

    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.tokenKind == .keyword(.let)

    return .decl(
      VarDecl_create(
        self.ctx,
        self.declContext,
        self.bridgedSourceLoc(for: node.bindingSpecifier),
        pattern,
        initializer,
        isStatic,
        isLet
      )
    )
  }

  public func visit(_ node: FunctionParameterSyntax) -> ASTNode {
    // FIXME: This location should be derived from the type repr.
    let specifierLoc: BridgedSourceLoc = nil

    let firstName: BridgedIdentifier
    if node.firstName.text != "_" {
      // Swift AST represents "_" as nil.
      firstName = node.firstName.bridgedIdentifier(in: self)
    } else {
      firstName = nil
    }
    let (secondName, secondNameLoc) = node.secondName.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      ParamDecl_create(
        self.ctx,
        self.declContext,
        specifierLoc,
        firstName,
        self.bridgedSourceLoc(for: node.firstName),
        secondName,
        secondNameLoc,
        self.visit(node.type).rawValue
      )
    )
  }

  public func visit(_ node: FunctionDeclSyntax) -> ASTNode {
    // FIXME: Compute this location
    let staticLoc: BridgedSourceLoc = nil

    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let parameters = node.signature.parameterClause.parameters.lazy.map { visit($0).rawValue }
    let out = FuncDecl_create(
      self.ctx,
      self.declContext,
      staticLoc,
      self.bridgedSourceLoc(for: node.funcKeyword),
      name,
      nameLoc,
      self.bridgedSourceLoc(for: node.signature.parameterClause.leftParen),
      parameters.bridgedArray(in: self),
      self.bridgedSourceLoc(for: node.signature.parameterClause.rightParen),
      self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.asyncSpecifier),
      self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.throwsSpecifier),
      self.visit(node.signature.returnClause?.type)?.rawValue
    )

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

extension ASTGenVisitor {
  @inline(__always)
  func visit(_ node: MemberBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self)
  }
}
