import CASTBridging

// Needed to use SyntaxTransformVisitor's visit method.
@_spi(SyntaxTransformVisitor)
import SwiftSyntax
import SwiftDiagnostics

// MARK: - TypeDecl

extension ASTGenVisitor {
  public func visit(_ node: TypeAliasDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      TypeAliasDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        typealiasKeywordLoc: self.bridgedSourceLoc(for: node.typealiasKeyword),
        name: name,
        nameLoc: nameLoc,
        genericParamList: self.visit(node.genericParameterClause)?.rawValue,
        equalLoc: self.bridgedSourceLoc(for: node.initializer.equal),
        underlyingType: self.visit(node.initializer.value).rawValue,
        genericWhereClause: self.visit(node.genericWhereClause)?.rawValue
      )
    )
  }

  public func visit(_ node: EnumDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = EnumDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      enumKeywordLoc: self.bridgedSourceLoc(for: node.enumKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func visit(_ node: StructDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = StructDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      structKeywordLoc: self.bridgedSourceLoc(for: node.structKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func visit(_ node: ClassDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = ClassDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.bridgedSourceLoc(for: node.classKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self),
      isActor: false
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func visit(_ node: ActorDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = ClassDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.bridgedSourceLoc(for: node.actorKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self),
      isActor: true
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  func visit(_ node: ProtocolDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let primaryAssociatedTypeNames = node.primaryAssociatedTypeClause?.primaryAssociatedTypes.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }

    let decl = ProtocolDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      protocolKeywordLoc: self.bridgedSourceLoc(for: node.protocolKeyword),
      name: name,
      nameLoc: nameLoc,
      primaryAssociatedTypeNames: primaryAssociatedTypeNames.bridgedArray(in: self),
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  func visit(_ node: AssociatedTypeDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      AssociatedTypeDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        associatedtypeKeywordLoc: self.bridgedSourceLoc(for: node.associatedtypeKeyword),
        name: name,
        nameLoc: nameLoc,
        inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
        defaultType: self.visit(node.initializer?.value)?.rawValue,
        genericWhereClause: self.visit(node.genericWhereClause)?.rawValue
      )
    )
  }
}

// MARK: - ExtensionDecl

extension ASTGenVisitor {
  func visit(_ node: ExtensionDeclSyntax) -> ASTNode {
    let decl = ExtensionDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      extensionKeywordLoc: self.bridgedSourceLoc(for: node.extensionKeyword),
      extendedType: self.visit(node.extendedType).rawValue,
      inheritedTypes: self.visit(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.visit(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }
}

// MARK: - EnumCaseDecl

extension ASTGenVisitor {
  func visit(_ node: EnumCaseElementSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      EnumElementDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        name: name,
        nameLoc: nameLoc,
        parameterList: self.visit(node.parameterClause)?.rawValue,
        equalsLoc: self.bridgedSourceLoc(for: node.rawValue?.equal),
        rawValue: self.visit(node.rawValue?.value)?.rawValue
      )
    )
  }

  func visit(_ node: EnumCaseDeclSyntax) -> ASTNode {
    .decl(
      EnumCaseDecl_create(
        declContext: self.declContext,
        caseKeywordLoc: self.bridgedSourceLoc(for: node.caseKeyword),
        elements: node.elements.lazy.map { self.visit($0).rawValue }.bridgedArray(in: self)
      )
    )
  }
}

// MARK: - AbstractStorageDecl

extension ASTGenVisitor {
  public func visit(_ node: VariableDeclSyntax) -> ASTNode {
    let pattern = visit(node.bindings.first!.pattern).rawValue
    let initializer = visit(node.bindings.first!.initializer!).rawValue

    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.tokenKind == .keyword(.let)

    return .decl(
      VarDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        bindingKeywordLoc: self.bridgedSourceLoc(for: node.bindingSpecifier),
        nameExpr: pattern,
        initializer: initializer,
        isStatic: isStatic,
        isLet: isLet
      )
    )
  }
}

// MARK: - AbstractFunctionDecl

extension ASTGenVisitor {
  public func visit(_ node: FunctionDeclSyntax) -> ASTNode {
    // FIXME: Compute this location
    let staticLoc: BridgedSourceLoc = nil

    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = FuncDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      staticLoc: staticLoc,
      funcKeywordLoc: self.bridgedSourceLoc(for: node.funcKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      parameterList: self.visit(node.signature.parameterClause).rawValue,
      asyncSpecifierLoc: self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.asyncSpecifier),
      throwsSpecifierLoc: self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.throwsSpecifier),
      returnType: self.visit(node.signature.returnClause?.type)?.rawValue,
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.visit(body).rawValue, ofDecl: decl.asDecl)
      }
    }

    return .decl(decl.asDecl)
  }

  func visit(_ node: InitializerDeclSyntax) -> ASTNode {
    let decl = ConstructorDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      initKeywordLoc: self.bridgedSourceLoc(for: node.initKeyword),
      failabilityMarkLoc: self.bridgedSourceLoc(for: node.optionalMark),
      isIUO: node.optionalMark?.tokenKind == .exclamationMark,
      genericParamList: self.visit(node.genericParameterClause)?.rawValue,
      parameterList: self.visit(node.signature.parameterClause).rawValue,
      asyncSpecifierLoc: self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.asyncSpecifier),
      throwsSpecifierLoc: self.bridgedSourceLoc(for: node.signature.effectSpecifiers?.throwsSpecifier),
      genericWhereClause: self.visit(node.genericWhereClause)?.rawValue
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.visit(body).rawValue, ofDecl: decl.asDecl)
      }
    }

    return .decl(decl.asDecl)
  }

  func visit(_ node: DeinitializerDeclSyntax) -> ASTNode {
    let decl = DestructorDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      deinitKeywordLoc: self.bridgedSourceLoc(for: node.deinitKeyword)
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.visit(body).rawValue, ofDecl: decl.asDecl)
      }
    }

    return .decl(decl.asDecl)
  }
}

// MARK: - OperatorDecl

extension BridgedOperatorFixity {
  fileprivate init?(from tokenKind: TokenKind) {
    switch tokenKind {
    case .keyword(.infix): self = .infix
    case .keyword(.prefix): self = .prefix
    case .keyword(.postfix): self = .postfix
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func visit(_ node: OperatorDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let (precedenceGroupName, precedenceGroupLoc) = (node.operatorPrecedenceAndTypes?.precedenceGroup).bridgedIdentifierAndSourceLoc(in: self)

    let fixity: BridgedOperatorFixity
    if let value = BridgedOperatorFixity(from: node.fixitySpecifier.tokenKind) {
      fixity = value
    } else {
      fixity = .infix
      self.diagnose(Diagnostic(node: node.fixitySpecifier, message: UnexpectedTokenKindError(token: node.fixitySpecifier)))
    }

    return .decl(
      OperatorDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        fixity: fixity,
        operatorKeywordLoc: self.bridgedSourceLoc(for: node.operatorKeyword),
        name: name,
        nameLoc: nameLoc,
        colonLoc: self.bridgedSourceLoc(for: node.operatorPrecedenceAndTypes?.colon),
        precedenceGroupName: precedenceGroupName,
        PrecedenceGroupLoc: precedenceGroupLoc
      )
    )
  }
}

// MARK: - PrecedenceGroupDecl

extension BridgedAssociativity {
  fileprivate init?(from tokenKind: TokenKind) {
    switch tokenKind {
    case .keyword(.none): self = .none
    case .keyword(.left): self = .left
    case .keyword(.right): self = .right
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func visit(_ node: PrecedenceGroupDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    struct PrecedenceGroupBody {
      var associativity: PrecedenceGroupAssociativitySyntax? = nil
      var assignment: PrecedenceGroupAssignmentSyntax? = nil
      var higherThanRelation: PrecedenceGroupRelationSyntax? = nil
      var lowerThanRelation: PrecedenceGroupRelationSyntax? = nil
    }

    func diagnoseDuplicateSyntax(_ duplicate: some SyntaxProtocol, original: some SyntaxProtocol) {
      self.diagnose(Diagnostic(node: duplicate, message: DuplicateSyntaxError(duplicate: duplicate, original: original)))
    }

    let body = node.groupAttributes.reduce(into: PrecedenceGroupBody()) { body, element in
      switch element {
      case .precedenceGroupRelation(let relation):
        let keyword = relation.higherThanOrLowerThanLabel
        switch keyword.tokenKind {
        case .keyword(.higherThan):
          if let current = body.higherThanRelation {
            diagnoseDuplicateSyntax(relation, original: current)
          } else {
            body.higherThanRelation = relation
          }
        case .keyword(.lowerThan):
          if let current = body.lowerThanRelation {
            diagnoseDuplicateSyntax(relation, original: current)
          } else {
            body.lowerThanRelation = relation
          }
        default:
          return self.diagnose(Diagnostic(node: keyword, message: UnexpectedTokenKindError(token: keyword)))
        }
      case .precedenceGroupAssignment(let assignment):
        if let current = body.assignment {
          diagnoseDuplicateSyntax(assignment, original: current)
        } else {
          body.assignment = assignment
        }
      case .precedenceGroupAssociativity(let associativity):
        if let current = body.associativity {
          diagnoseDuplicateSyntax(node, original: current)
        } else {
          body.associativity = associativity
        }
      }
    }

    let associativityValue: BridgedAssociativity
    if let token = body.associativity?.value {
      if let value = BridgedAssociativity(from: token.tokenKind) {
        associativityValue = value
      } else {
        self.diagnose(Diagnostic(node: token, message: UnexpectedTokenKindError(token: token)))
        associativityValue = .none
      }
    } else {
      associativityValue = .none
    }

    let assignmentValue: Bool
    if let token = body.assignment?.value {
      if token.tokenKind == .keyword(.true) {
        assignmentValue = true
      } else {
        self.diagnose(Diagnostic(node: token, message: UnexpectedTokenKindError(token: token)))
        assignmentValue = false
      }
    } else {
      assignmentValue = false
    }

    return .decl(
      PrecedenceGroupDecl_create(
        declContext: self.declContext,
        precedencegroupKeywordLoc: self.bridgedSourceLoc(for: node.precedencegroupKeyword),
        name: name,
        nameLoc: nameLoc,
        leftBraceLoc: self.bridgedSourceLoc(for: node.leftBrace),
        associativityLabelLoc: self.bridgedSourceLoc(for: body.associativity?.associativityLabel),
        associativityValueLoc: self.bridgedSourceLoc(for: body.associativity?.value),
        associativity: associativityValue,
        assignmentLabelLoc: self.bridgedSourceLoc(for: body.assignment?.assignmentLabel),
        assignmentValueLoc: self.bridgedSourceLoc(for: body.assignment?.value),
        isAssignment: assignmentValue,
        higherThanKeywordLoc: self.bridgedSourceLoc(for: body.higherThanRelation?.higherThanOrLowerThanLabel),
        higherThanNames: self.visit(body.higherThanRelation?.precedenceGroups),
        lowerThanKeywordLoc: self.bridgedSourceLoc(for: body.lowerThanRelation?.higherThanOrLowerThanLabel),
        lowerThanNames: self.visit(body.lowerThanRelation?.precedenceGroups),
        rightBraceLoc: self.bridgedSourceLoc(for: node.rightBrace)
      )
    )
  }
}

// MARK: - ImportDecl

extension BridgedImportKind {
  fileprivate init?(from tokenKind: TokenKind) {
    switch tokenKind {
    case .keyword(.typealias): self = .type
    case .keyword(.struct): self = .struct
    case .keyword(.class): self = .class
    case .keyword(.enum): self = .enum
    case .keyword(.protocol): self = .protocol
    case .keyword(.var), .keyword(.let): self = .var
    case .keyword(.func): self = .func
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func visit(_ node: ImportDeclSyntax) -> ASTNode {
    let importKind: BridgedImportKind
    if let specifier = node.importKindSpecifier {
      if let value = BridgedImportKind(from: specifier.tokenKind) {
        importKind = value
      } else {
        self.diagnose(Diagnostic(node: specifier, message: UnexpectedTokenKindError(token: specifier)))
        importKind = .module
      }
    } else {
      importKind = .module
    }

    return .decl(
      ImportDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        importKeywordLoc: self.bridgedSourceLoc(for: node.importKeyword),
        importKind: importKind,
        importKindLoc: self.bridgedSourceLoc(for: node.importKindSpecifier),
        path: node.path.lazy.map {
          $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
        }.bridgedArray(in: self)
      )
    )
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

  @inline(__always)
  func visit(_ node: PrecedenceGroupNameListSyntax) -> BridgedArrayRef {
    node.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }.bridgedArray(in: self)
  }
}
