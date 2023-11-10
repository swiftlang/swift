//===--- Decls.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

// MARK: - TypeDecl

extension ASTGenVisitor {
  public func generate(_ node: TypeAliasDeclSyntax) -> BridgedTypeAliasDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      typealiasKeywordLoc: node.typealiasKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      equalLoc: node.initializer.equal.bridgedSourceLoc(in: self),
      underlyingType: self.generate(node.initializer.value),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable
    )
  }

  public func generate(_ node: EnumDeclSyntax) -> BridgedNominalTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = BridgedEnumDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      enumKeywordLoc: node.enumKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      )
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }

  public func generate(_ node: StructDeclSyntax) -> BridgedNominalTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = BridgedStructDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      structKeywordLoc: node.structKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      )
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }

  public func generate(_ node: ClassDeclSyntax) -> BridgedNominalTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: node.classKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      ),
      isActor: false
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }

  public func generate(_ node: ActorDeclSyntax) -> BridgedNominalTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: node.actorKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      ),
      isActor: true
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }

  func generate(_ node: ProtocolDeclSyntax) -> BridgedNominalTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let primaryAssociatedTypeNames = node.primaryAssociatedTypeClause?.primaryAssociatedTypes.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }

    let decl = BridgedProtocolDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      protocolKeywordLoc: node.protocolKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      primaryAssociatedTypeNames: primaryAssociatedTypeNames.bridgedArray(in: self),
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      )
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }

  func generate(_ node: AssociatedTypeDeclSyntax) -> BridgedAssociatedTypeDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      associatedtypeKeywordLoc: node.associatedtypeKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      defaultType: self.generate(node.initializer?.value).asNullable,
      genericWhereClause: self.generate(node.genericWhereClause).asNullable
    )
  }
}

// MARK: - ExtensionDecl

extension ASTGenVisitor {
  func generate(_ node: ExtensionDeclSyntax) -> BridgedExtensionDecl {
    let decl = BridgedExtensionDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      extensionKeywordLoc: node.extensionKeyword.bridgedSourceLoc(in: self),
      extendedType: self.generate(node.extendedType),
      inheritedTypes: self.generate(node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(node.genericWhereClause).asNullable,
      braceRange: BridgedSourceRange(
        startToken: node.memberBlock.leftBrace,
        endToken: node.memberBlock.rightBrace,
        in: self
      )
    )

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(node.memberBlock.members))
    }

    return decl
  }
}

// MARK: - EnumCaseDecl

extension ASTGenVisitor {
  func generate(_ node: EnumCaseElementSyntax) -> BridgedEnumElementDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      name: name,
      nameLoc: nameLoc,
      parameterList: self.generate(node.parameterClause).asNullable,
      equalsLoc: (node.rawValue?.equal).bridgedSourceLoc(in: self),
      rawValue: self.generate(node.rawValue?.value).asNullable
    )
  }

  func generate(_ node: EnumCaseDeclSyntax) -> BridgedEnumCaseDecl {
    .createParsed(
      declContext: self.declContext,
      caseKeywordLoc: node.caseKeyword.bridgedSourceLoc(in: self),
      elements: node.elements.lazy.map(self.generate).bridgedArray(in: self)
    )
  }
}

// MARK: - AbstractStorageDecl

extension ASTGenVisitor {
  public func generate(_ node: VariableDeclSyntax) -> BridgedPatternBindingDecl {
    let pattern = generate(node.bindings.first!.pattern)
    let initializer = generate(node.bindings.first!.initializer!)

    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.tokenKind == .keyword(.let)

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      bindingKeywordLoc: node.bindingSpecifier.bridgedSourceLoc(in: self),
      nameExpr: pattern.castToExpr,
      initializer: initializer,
      isStatic: isStatic,
      isLet: isLet
    )
  }
}

// MARK: - AbstractFunctionDecl

extension ASTGenVisitor {
  public func generate(_ node: FunctionDeclSyntax) -> BridgedFuncDecl {
    // FIXME: Compute this location
    let staticLoc: BridgedSourceLoc = nil

    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    let decl = BridgedFuncDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      staticLoc: staticLoc,
      funcKeywordLoc: node.funcKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      parameterList: self.generate(node.signature.parameterClause),
      asyncSpecifierLoc: (node.signature.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsSpecifierLoc: (node.signature.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(node.signature.effectSpecifiers?.thrownError?.type).asNullable,
      returnType: self.generate(node.signature.returnClause?.type).asNullable,
      genericWhereClause: self.generate(node.genericWhereClause).asNullable
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(body))
      }
    }

    return decl
  }

  func generate(_ node: InitializerDeclSyntax) -> BridgedConstructorDecl {
    let decl = BridgedConstructorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      initKeywordLoc: node.initKeyword.bridgedSourceLoc(in: self),
      failabilityMarkLoc: node.optionalMark.bridgedSourceLoc(in: self),
      isIUO: node.optionalMark?.tokenKind == .exclamationMark,
      genericParamList: self.generate(node.genericParameterClause).asNullable,
      parameterList: self.generate(node.signature.parameterClause),
      asyncSpecifierLoc: (node.signature.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsSpecifierLoc: (node.signature.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(node.signature.effectSpecifiers?.thrownError?.type).asNullable,
      genericWhereClause: self.generate(node.genericWhereClause).asNullable
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(body))
      }
    }

    return decl
  }

  func generate(_ node: DeinitializerDeclSyntax) -> BridgedDestructorDecl {
    let decl = BridgedDestructorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      deinitKeywordLoc: node.deinitKeyword.bridgedSourceLoc(in: self)
    )

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(body))
      }
    }

    return decl
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
  func generate(_ node: OperatorDeclSyntax) -> BridgedOperatorDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)
    let (precedenceGroupName, precedenceGroupLoc) = (node.operatorPrecedenceAndTypes?.precedenceGroup)
      .bridgedIdentifierAndSourceLoc(in: self)

    let fixity: BridgedOperatorFixity
    if let value = BridgedOperatorFixity(from: node.fixitySpecifier.tokenKind) {
      fixity = value
    } else {
      fixity = .infix
      self.diagnose(
        Diagnostic(node: node.fixitySpecifier, message: UnexpectedTokenKindError(token: node.fixitySpecifier))
      )
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      fixity: fixity,
      operatorKeywordLoc: node.operatorKeyword.bridgedSourceLoc(in: self),
      name: name,
      nameLoc: nameLoc,
      colonLoc: (node.operatorPrecedenceAndTypes?.colon).bridgedSourceLoc(in: self),
      precedenceGroupName: precedenceGroupName,
      precedenceGroupLoc: precedenceGroupLoc
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
  func generate(_ node: PrecedenceGroupDeclSyntax) -> BridgedPrecedenceGroupDecl {
    let (name, nameLoc) = node.name.bridgedIdentifierAndSourceLoc(in: self)

    struct PrecedenceGroupBody {
      var associativity: PrecedenceGroupAssociativitySyntax? = nil
      var assignment: PrecedenceGroupAssignmentSyntax? = nil
      var higherThanRelation: PrecedenceGroupRelationSyntax? = nil
      var lowerThanRelation: PrecedenceGroupRelationSyntax? = nil
    }

    func diagnoseDuplicateSyntax(_ duplicate: some SyntaxProtocol, original: some SyntaxProtocol) {
      self.diagnose(
        Diagnostic(node: duplicate, message: DuplicateSyntaxError(duplicate: duplicate, original: original))
      )
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

    return .createParsed(
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
  func generate(_ node: ImportDeclSyntax) -> BridgedImportDecl {
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

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      importKeywordLoc: node.importKeyword.bridgedSourceLoc(in: self),
      importKind: importKind,
      importKindLoc: node.importKindSpecifier.bridgedSourceLoc(in: self),
      path: node.path.lazy.map {
        $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
      }.bridgedArray(in: self)
    )
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(_ node: MemberBlockItemListSyntax) -> BridgedArrayRef {
    node.lazy.map(self.generate).bridgedArray(in: self)
  }

  @inline(__always)
  func generate(_ node: InheritedTypeListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate($0.type) }.bridgedArray(in: self)
  }

  @inline(__always)
  func generate(_ node: PrecedenceGroupNameListSyntax) -> BridgedArrayRef {
    node.lazy.map {
      $0.name.bridgedIdentifierAndSourceLoc(in: self) as BridgedIdentifierAndSourceLoc
    }.bridgedArray(in: self)
  }
}
