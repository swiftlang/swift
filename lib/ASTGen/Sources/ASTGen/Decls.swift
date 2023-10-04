import CASTBridging

@_spi(ExperimentalLanguageFeatures)
import SwiftSyntax
import SwiftDiagnostics

// MARK: - TypeDecl

extension ASTGenVisitor {
  public func generate(_ node: TypeAliasDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      TypeAliasDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        typealiasKeywordLoc: node.typealiasKeyword.bridgedSourceLoc(in: self),
        name: name,
        nameLoc: nameLoc,
        genericParamList: self.generate(node.genericParameterClause)?.rawValue,
        equalLoc: node.initializer.equal.bridgedSourceLoc(in: self),
        underlyingType: self.generate(node.initializer.value).rawValue,
        genericWhereClause: self.generate(node.genericWhereClause)?.rawValue
      )
    )
  }

  public func generate(_ node: EnumDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = EnumDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      enumKeywordLoc: node.enumKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func generate(_ node: StructDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = StructDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      structKeywordLoc: node.structKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func generate(_ node: ClassDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = ClassDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      classKeywordLoc: node.classKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self),
      isActor: false
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  public func generate(_ node: ActorDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = ClassDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      classKeywordLoc: node.actorKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self),
      isActor: true
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  func generate(_ node: ProtocolDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let primaryAssociatedTypeNames = node.primaryAssociatedTypeClause?.primaryAssociatedTypes.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }

    let decl = ProtocolDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      protocolKeywordLoc: node.protocolKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      primaryAssociatedTypeNames: primaryAssociatedTypeNames.bridgedArray(in: self),
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }

  func generate(_ node: AssociatedTypeDeclSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      AssociatedTypeDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        associatedtypeKeywordLoc: node.associatedtypeKeyword.bridgedSourceLoc(in: self),
        name: name,
        nameLoc: nameLoc,
        inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
        defaultType: self.generate(node.initializer?.value)?.rawValue,
        genericWhereClause: self.generate(node.genericWhereClause)?.rawValue
      )
    )
  }
}

// MARK: - ExtensionDecl

extension ASTGenVisitor {
  func generate(_ node: ExtensionDeclSyntax) -> ASTNode {
    let decl = ExtensionDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      extensionKeywordLoc: node.extensionKeyword.bridgedSourceLoc(in: self),
      extendedType: self.generate(node.extendedType).rawValue,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue,
      braceRange: BridgedSourceRange(startToken: node.memberBlock.leftBrace, endToken: node.memberBlock.rightBrace, in: self)
    )

    self.withDeclContext(decl.asDeclContext) {
      IterableDeclContext_setParsedMembers(self.generate(node.memberBlock.members), ofDecl: decl.asDecl)
    }

    return .decl(decl.asDecl)
  }
}

// MARK: - EnumCaseDecl

extension ASTGenVisitor {
  func generate(_ node: EnumCaseElementSyntax) -> ASTNode {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .decl(
      EnumElementDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        name: name,
        nameLoc: nameLoc,
        parameterList: self.generate(node.parameterClause)?.rawValue,
        equalsLoc: (node.rawValue?.equal).bridgedSourceLoc(in: self),
        rawValue: self.generate(node.rawValue?.value)?.rawValue
      )
    )
  }

  func generate(_ node: EnumCaseDeclSyntax) -> ASTNode {
    .decl(
      EnumCaseDecl_create(
        declContext: self.declContext,
        caseKeywordLoc: node.caseKeyword.bridgedSourceLoc(in: self),
        elements: node.elements.lazy.map { self.generate($0).rawValue }.bridgedArray(in: self)
      )
    )
  }
}

// MARK: - AbstractStorageDecl

extension ASTGenVisitor {
  public func generate(_ node: VariableDeclSyntax) -> ASTNode {
    let pattern = generate(node.bindings.first!.pattern).rawValue
    let initializer = generate(node.bindings.first!.initializer!).rawValue

    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.tokenKind == .keyword(.let)

    return .decl(
      VarDecl_create(
        astContext: self.ctx,
        declContext: self.declContext,
        bindingKeywordLoc: node.bindingSpecifier.bridgedSourceLoc(in: self),
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
  public func generate(_ node: FunctionDeclSyntax) -> ASTNode {
    // FIXME: Compute this location
    let staticLoc: BridgedSourceLoc = nil

    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = FuncDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      staticLoc: staticLoc,
      funcKeywordLoc: node.funcKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      parameterList: self.generate(node.signature.parameterClause).rawValue,
      asyncSpecifierLoc: (node.signature.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsSpecifierLoc: (node.signature.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(node.signature.effectSpecifiers?.thrownError?.type)?.rawValue,
      returnType: self.generate(node.signature.returnClause?.type)?.rawValue,
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.generate(body).rawValue, ofDecl: decl.asDecl)
      }
    }

    return .decl(decl.asDecl)
  }

  func generate(_ node: InitializerDeclSyntax) -> ASTNode {
    let decl = ConstructorDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      initKeywordLoc: node.initKeyword.bridgedSourceLoc(in: self),
      failabilityMarkLoc: node.optionalMark.bridgedSourceLoc(in: self),
      isIUO: node.optionalMark?.tokenKind == .exclamationMark,
      genericParamList: self.generate(node.genericParameterClause)?.rawValue,
      parameterList: self.generate(node.signature.parameterClause).rawValue,
      asyncSpecifierLoc: (node.signature.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsSpecifierLoc: (node.signature.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(node.signature.effectSpecifiers?.thrownError?.type)?.rawValue,
      genericWhereClause: self.generate(node.genericWhereClause)?.rawValue
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.generate(body).rawValue, ofDecl: decl.asDecl)
      }
    }

    return .decl(decl.asDecl)
  }

  func generate(_ node: DeinitializerDeclSyntax) -> ASTNode {
    let decl = DestructorDecl_create(
      astContext: self.ctx,
      declContext: self.declContext,
      deinitKeywordLoc: node.deinitKeyword.bridgedSourceLoc(in: self)
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        AbstractFunctionDecl_setBody(self.generate(body).rawValue, ofDecl: decl.asDecl)
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
  func generate(_ node: OperatorDeclSyntax) -> ASTNode {
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
        operatorKeywordLoc: node.operatorKeyword.bridgedSourceLoc(in: self),
        name: name,
        nameLoc: nameLoc,
        colonLoc: (node.operatorPrecedenceAndTypes?.colon).bridgedSourceLoc(in: self),
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
  func generate(_ node: PrecedenceGroupDeclSyntax) -> ASTNode {
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
        precedencegroupKeywordLoc: node.precedencegroupKeyword.bridgedSourceLoc(in: self),
        name: name,
        nameLoc: nameLoc,
        leftBraceLoc: node.leftBrace.bridgedSourceLoc(in: self),
        associativityLabelLoc: (body.associativity?.associativityLabel).bridgedSourceLoc(in: self),
        associativityValueLoc: (body.associativity?.value).bridgedSourceLoc(in: self),
        associativity: associativityValue,
        assignmentLabelLoc: (body.assignment?.assignmentLabel).bridgedSourceLoc(in: self),
        assignmentValueLoc: (body.assignment?.value).bridgedSourceLoc(in: self),
        isAssignment: assignmentValue,
        higherThanKeywordLoc: (body.higherThanRelation?.higherThanOrLowerThanLabel).bridgedSourceLoc(in: self),
        higherThanNames: self.generate(body.higherThanRelation?.precedenceGroups),
        lowerThanKeywordLoc: (body.lowerThanRelation?.higherThanOrLowerThanLabel).bridgedSourceLoc(in: self),
        lowerThanNames: self.generate(body.lowerThanRelation?.precedenceGroups),
        rightBraceLoc: node.rightBrace.bridgedSourceLoc(in: self)
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
  func generate(_ node: ImportDeclSyntax) -> ASTNode {
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
        importKeywordLoc: node.importKeyword.bridgedSourceLoc(in: self),
        importKind: importKind,
        importKindLoc: node.importKindSpecifier.bridgedSourceLoc(in: self),
        path: node.path.lazy.map {
          $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
        }.bridgedArray(in: self)
      )
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(_ node: MemberBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate($0).rawValue }.bridgedArray(in: self)
  }

  @inline(__always)
  func generate(_ node: InheritedTypeListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate($0.type).rawValue }.bridgedArray(in: self)
  }

  @inline(__always)
  func generate(_ node: PrecedenceGroupNameListSyntax) -> BridgedArrayRef {
    node.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }.bridgedArray(in: self)
  }
}
