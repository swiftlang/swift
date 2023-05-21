import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax

// MARK: - TypeDecl

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

    let decl = StructDecl_create(
      self.ctx,
      self.declContext,
      self.bridgedSourceLoc(for: node.structKeyword),
      name,
      nameLoc,
      self.visit(node.genericParameterClause)?.rawValue,
      self.visit(node.inheritanceClause?.inheritedTypes),
      BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func visit(_ node: ClassDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = ClassDecl_create(
      self.ctx,
      self.declContext,
      self.bridgedSourceLoc(for: node.classKeyword),
      name,
      nameLoc,
      self.visit(node.genericParameterClause)?.rawValue,
      self.visit(node.inheritanceClause?.inheritedTypes),
      BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  func visit(_ node: ProtocolDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let primaryAssociatedTypeNames = node.primaryAssociatedTypeClause?.primaryAssociatedTypes.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }

    let decl = ProtocolDecl_create(
      self.ctx,
      self.declContext,
      self.bridgedSourceLoc(for: node.protocolKeyword),
      name,
      nameLoc,
      primaryAssociatedTypeNames.bridgedArray(in: self),
      self.visit(node.inheritanceClause?.inheritedTypes),
      BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), decl.asDecl)
    }

    return .decl(decl.asDecl)
  }
}

extension ASTGenVisitor {
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

  public func visit(_ node: FunctionDeclSyntax) -> ASTNode {
    // FIXME: Compute this location
    let staticLoc: BridgedSourceLoc = nil

    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let out = FuncDecl_create(
      self.ctx,
      self.declContext,
      staticLoc,
      self.bridgedSourceLoc(for: node.funcKeyword),
      name,
      nameLoc,
      self.visit(node.genericParameterClause)?.rawValue,
      self.visit(node.signature.parameterClause).rawValue,
      self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.asyncSpecifier),
      self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.throwsSpecifier),
      self.visit(node.signature.returnClause?.type)?.rawValue
    )

    if let body = node.body {
      self.withDeclContext(out.declContext) {
        FuncDecl_setBody(out.funcDecl, self.visit(body).rawValue)
      }
    }

    return .decl(out.decl)
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func visit(_ node: MemberBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self)
  }

  @inline(__always)
  func visit(_ node: InheritedTypeListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.visit($0.type).rawValue }.bridgedArray(in: self)
  }
}
