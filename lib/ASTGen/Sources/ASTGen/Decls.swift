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
@_spi(ExperimentalLanguageFeatures) @_spi(RawSyntax) import SwiftSyntax

// MARK: - TypeDecl

extension ASTGenVisitor {
  func generate(decl node: DeclSyntax) -> BridgedDecl {
    switch node.as(DeclSyntaxEnum.self) {
    case .accessorDecl:
      fatalError("Should be generated as a part of another decl")
    case .actorDecl(let node):
      return self.generate(actorDecl: node).asDecl
    case .associatedTypeDecl(let node):
      return self.generate(associatedTypeDecl: node).asDecl
    case .classDecl(let node):
      return self.generate(classDecl: node).asDecl
    case .deinitializerDecl(let node):
      return self.generate(deinitializerDecl: node).asDecl
    case .editorPlaceholderDecl:
      break
    case .enumCaseDecl(let node):
      return self.generate(enumCaseDecl: node).asDecl
    case .enumDecl(let node):
      return self.generate(enumDecl: node).asDecl
    case .extensionDecl(let node):
      return self.generate(extensionDecl: node).asDecl
    case .functionDecl(let node):
      return self.generate(functionDecl: node).asDecl
    case .ifConfigDecl:
      fatalError("Should have been handled by the caller")
    case .importDecl(let node):
      return self.generate(importDecl: node).asDecl
    case .initializerDecl(let node):
      return self.generate(initializerDecl: node).asDecl
    case .macroDecl:
      break
    case .macroExpansionDecl:
      break
    case .missingDecl:
      break
    case .operatorDecl(let node):
      return self.generate(operatorDecl: node).asDecl
    case .poundSourceLocation:
      break
    case .precedenceGroupDecl(let node):
      return self.generate(precedenceGroupDecl: node).asDecl
    case .protocolDecl(let node):
      return self.generate(protocolDecl: node).asDecl
    case .structDecl(let node):
      return self.generate(structDecl: node).asDecl
    case .subscriptDecl(let node):
      return self.generate(subscriptDecl: node).asDecl
    case .typeAliasDecl(let node):
      return self.generate(typeAliasDecl: node).asDecl
    case .variableDecl(let node):
      return self.generate(variableDecl: node).asDecl
#if RESILIENT_SWIFT_SYNTAX
    @unknown default:
      fatalError()
#endif
    }
    return self.generateWithLegacy(node)
  }

  func generate(memberBlockItem node: MemberBlockItemSyntax) -> BridgedDecl {
    // TODO: Set semicolon loc.
    generate(decl: node.decl)
  }

  func generate(typeAliasDecl node: TypeAliasDeclSyntax) -> BridgedTypeAliasDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedTypeAliasDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      typealiasKeywordLoc: self.generateSourceLoc(node.typealiasKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      equalLoc: self.generateSourceLoc(node.initializer.equal),
      underlyingType: self.generate(type: node.initializer.value),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.setAttrs(attrs.attributes)
    return decl
  }

  func generate(enumDecl node: EnumDeclSyntax) -> BridgedNominalTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedEnumDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      enumKeywordLoc: self.generateSourceLoc(node.enumKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      )
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }

  func generate(structDecl node: StructDeclSyntax) -> BridgedNominalTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedStructDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      structKeywordLoc: self.generateSourceLoc(node.structKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      )
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }

  func generate(classDecl node: ClassDeclSyntax) -> BridgedNominalTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.generateSourceLoc(node.classKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      ),
      isActor: false
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }

  func generate(actorDecl node: ActorDeclSyntax) -> BridgedNominalTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.generateSourceLoc(node.actorKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      ),
      isActor: true
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }

  func generate(protocolDecl node: ProtocolDeclSyntax) -> BridgedNominalTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)
    let primaryAssociatedTypeNames = node.primaryAssociatedTypeClause?.primaryAssociatedTypes.lazy.map {
      self.generateLocatedIdentifier($0.name)
    }

    let decl = BridgedProtocolDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      protocolKeywordLoc: self.generateSourceLoc(node.protocolKeyword),
      name: name,
      nameLoc: nameLoc,
      primaryAssociatedTypeNames: primaryAssociatedTypeNames.bridgedArray(in: self),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      )
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }

  func generate(associatedTypeDecl node: AssociatedTypeDeclSyntax) -> BridgedAssociatedTypeDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedAssociatedTypeDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      associatedtypeKeywordLoc: self.generateSourceLoc(node.associatedtypeKeyword),
      name: name,
      nameLoc: nameLoc,
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      defaultType: self.generate(type: node.initializer?.value),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.setAttrs(attrs.attributes)
    return decl
  }
}

// MARK: - ExtensionDecl

extension ASTGenVisitor {
  func generate(extensionDecl node: ExtensionDeclSyntax) -> BridgedExtensionDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let decl = BridgedExtensionDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      extensionKeywordLoc: self.generateSourceLoc(node.extensionKeyword),
      extendedType: self.generate(type: node.extendedType),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateSourceRange(
        start: node.memberBlock.leftBrace,
        end: node.memberBlock.rightBrace
      )
    )
    decl.asDecl.setAttrs(attrs.attributes)

    self.withDeclContext(decl.asDeclContext) {
      decl.setParsedMembers(self.generate(memberBlockItemList: node.memberBlock.members))
    }

    return decl
  }
}

// MARK: - EnumCaseDecl

extension ASTGenVisitor {
  func generate(enumCaseElement node: EnumCaseElementSyntax) -> BridgedEnumElementDecl {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      name: name,
      nameLoc: nameLoc,
      parameterList: self.generate(enumCaseParameterClause: node.parameterClause),
      equalsLoc: self.generateSourceLoc(node.rawValue?.equal),
      rawValue: self.generate(expr: node.rawValue?.value)
    )
  }

  func generate(enumCaseDecl node: EnumCaseDeclSyntax) -> BridgedEnumCaseDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)

    // All attributes goes to each element.
    let elements = node.elements.lazy.map({ elem -> BridgedEnumElementDecl in
      let elemDecl = self.generate(enumCaseElement: elem)
      elemDecl.asDecl.setAttrs(attrs.attributes)
      return elemDecl
    })
    return .createParsed(
      declContext: self.declContext,
      caseKeywordLoc: self.generateSourceLoc(node.caseKeyword),
      elements: elements.bridgedArray(in: self)
    )
  }
}

// MARK: - AbstractStorageDecl

extension ASTGenVisitor {
  private func generate(accessorSpecifier specifier: TokenSyntax) -> BridgedAccessorKind? {
    switch specifier.keywordKind {
    case .get:
      return .get
    case .set:
      return .set
    case .didSet:
      return .didSet
    case .willSet:
      return .willSet
    case .unsafeAddress:
      return .address
    case .unsafeMutableAddress:
      return .mutableAddress
    case ._read:
      return .read
    case ._modify:
      return .modify
    case .`init`:
      return .`init`
    default:
      self.diagnose(Diagnostic(node: specifier, message: UnknownAccessorSpecifierError(specifier)))
      return nil
    }
  }

  private func generate(
    accessorDecl node: AccessorDeclSyntax,
    for storage: BridgedAbstractStorageDecl
  ) -> BridgedAccessorDecl? {
    // TODO: Attributes and modifier.

    guard let kind = self.generate(accessorSpecifier: node.accessorSpecifier) else {
      // TODO: We could potentially recover if this is the first accessor by treating
      // it as an implicit getter.
      return nil
    }
    let accessor = BridgedAccessorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      kind: kind,
      storage: storage,
      declLoc: self.generateSourceLoc(node.accessorSpecifier),
      accessorKeywordLoc: self.generateSourceLoc(node.accessorSpecifier),
      parameterList: self.generate(accessorParameters: node.parameters),
      asyncSpecifierLoc: self.generateSourceLoc(node.effectSpecifiers?.asyncSpecifier),
      throwsSpecifierLoc: self.generateSourceLoc(node.effectSpecifiers?.throwsClause),
      thrownType: self.generate(type: node.effectSpecifiers?.thrownError)
    )
    if let body = node.body {
      self.withDeclContext(accessor.asDeclContext) {
        accessor.setParsedBody(self.generate(codeBlock: body))
      }
    }
    return accessor
  }

  private func generate(
    accessorBlock: AccessorBlockSyntax,
    for storage: BridgedAbstractStorageDecl
  ) -> BridgedAccessorRecord {
    let leftBrace = self.generateSourceLoc(accessorBlock.leftBrace)
    let rightBrace = self.generateSourceLoc(accessorBlock.rightBrace)

    // FIXME: We need to handle the diagnostics in ParsedAccessors::classify,
    // or defer them to the type-checker.
    switch accessorBlock.accessors {
    case .accessors(let accessors):
      return BridgedAccessorRecord(
        lBraceLoc: leftBrace,
        accessors: accessors.lazy.compactMap {
          self.generate(accessorDecl: $0, for: storage)
        }.bridgedArray(in: self),
        rBraceLoc: rightBrace
      )
    case .getter(let codeBlock):
      let accessor = BridgedAccessorDecl.createParsed(
        self.ctx,
        declContext: self.declContext,
        kind: .get,
        storage: storage,
        declLoc: leftBrace,
        accessorKeywordLoc: nil,
        parameterList: nil,
        asyncSpecifierLoc: nil,
        throwsSpecifierLoc: nil,
        thrownType: nil
      )
      self.withDeclContext(accessor.asDeclContext) {
        let brace = BridgedBraceStmt.createParsed(
          self.ctx,
          lBraceLoc: leftBrace,
          elements: self.generate(codeBlockItemList: codeBlock),
          rBraceLoc: rightBrace
        )
        accessor.setParsedBody(brace)
      }
      return BridgedAccessorRecord(
        lBraceLoc: leftBrace,
        accessors: CollectionOfOne(accessor).bridgedArray(in: self),
        rBraceLoc: rightBrace
      )
#if RESILIENT_SWIFT_SYNTAX
    @unknown default:
      fatalError()
#endif
    }
  }

  func generate(patternBinding binding: PatternBindingSyntax) -> BridgedPatternBindingEntry {
    let pattern = generate(pattern: binding.pattern)
    let equalLoc = generateSourceLoc(binding.initializer?.equal)

    var initExpr: BridgedExpr?
    var initContext: BridgedPatternBindingInitializer?
    if let initializer = binding.initializer {
      // Create a PatternBindingInitializer if we're not in a local context (this
      // ensures that property initializers are correctly treated as being in a
      // local context).
      if !self.declContext.isLocalContext {
        initContext = .create(declContext: self.declContext)
      }
      initExpr = withDeclContext(initContext?.asDeclContext ?? self.declContext) {
        generate(expr: initializer.value)
      }
    }
    if let accessors = binding.accessorBlock {
      // FIXME: We ought to have a better way of converting from the Nullable
      // wrapper back to Optional.
      if let primaryVar = pattern.singleVar.raw.map(BridgedVarDecl.init) {
        let storage = primaryVar.asAbstractStorageDecl
        storage.setAccessors(generate(accessorBlock: accessors, for: storage))
      } else {
        self.diagnose(Diagnostic(node: binding.pattern, message: NonTrivialPatternForAccessorError()))
      }
    }
    return BridgedPatternBindingEntry(
      pattern: pattern,
      equalLoc: equalLoc,
      init: initExpr.asNullable,
      initContext: initContext.asNullable
    )
  }

  private func generateBindingEntries(for node: VariableDeclSyntax) -> BridgedArrayRef {
    var propagatedType: BridgedTypeRepr?
    var entries: [BridgedPatternBindingEntry] = []

    // Generate the bindings in reverse, keeping track of the TypeRepr to
    // propagate to earlier patterns if needed.
    for binding in node.bindings.reversed() {
      var entry = self.generate(patternBinding: binding)

      // We can potentially propagate a type annotation back if we don't have an initializer, and are a bare NamedPattern.
      let canPropagateType = binding.initializer == nil && binding.pattern.is(IdentifierPatternSyntax.self)
      if !canPropagateType {
        propagatedType = nil
      }

      // If we have a type annotation, wrap the pattern in it.
      if let typeAnnotation = binding.typeAnnotation {
        let typeRepr = self.generate(type: typeAnnotation.type)
        if canPropagateType {
          propagatedType = typeRepr
        }
        entry.pattern =
          BridgedTypedPattern.createParsed(
            self.ctx,
            pattern: entry.pattern,
            type: typeRepr
          ).asPattern
      } else if let propagatedType = propagatedType {
        entry.pattern =
          BridgedTypedPattern.createPropagated(
            self.ctx,
            pattern: entry.pattern,
            type: propagatedType
          ).asPattern
      }
      entries.append(entry)
    }
    return entries.reversed().bridgedArray(in: self)
  }

  func generate(variableDecl node: VariableDeclSyntax) -> BridgedPatternBindingDecl {
    // TODO: Attributes and modifiers
    let isStatic = false  // TODO: compute this
    let isLet = node.bindingSpecifier.keywordKind == .let

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      bindingKeywordLoc: self.generateSourceLoc(node.bindingSpecifier),
      entries: self.generateBindingEntries(for: node),
      isStatic: isStatic,
      isLet: isLet
    )
  }

  func generate(subscriptDecl node: SubscriptDeclSyntax) -> BridgedSubscriptDecl {
    let attrs = generateDeclAttributes(node, allowStatic: true)

    let subscriptDecl = BridgedSubscriptDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      staticLoc: attrs.staticLoc,
      staticSpelling: attrs.staticSpelling,
      subscriptKeywordLoc: self.generateSourceLoc(node.subscriptKeyword),
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: self.generate(functionParameterClause: node.parameterClause, forSubscript: true),
      arrowLoc: self.generateSourceLoc(node.returnClause.arrow),
      returnType: self.generate(type: node.returnClause.type)
    )
    subscriptDecl.asDecl.setAttrs(attrs.attributes)

    if let accessors = node.accessorBlock {
      let storage = subscriptDecl.asAbstractStorageDecl
      storage.setAccessors(generate(accessorBlock: accessors, for: storage))
    }
    return subscriptDecl
  }
}

// MARK: - AbstractFunctionDecl

extension ASTGenVisitor {
  func generate(functionDecl node: FunctionDeclSyntax) -> BridgedFuncDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: true)

    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

    let decl = BridgedFuncDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      staticLoc: attrs.staticLoc,
      staticSpelling: attrs.staticSpelling,
      funcKeywordLoc: self.generateSourceLoc(node.funcKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: self.generate(functionParameterClause: node.signature.parameterClause, forSubscript: false),
      asyncSpecifierLoc: self.generateSourceLoc(node.signature.effectSpecifiers?.asyncSpecifier),
      throwsSpecifierLoc: self.generateSourceLoc(node.signature.effectSpecifiers?.throwsClause?.throwsSpecifier),
      thrownType: self.generate(type: node.signature.effectSpecifiers?.thrownError),
      returnType: self.generate(type: node.signature.returnClause?.type),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.setAttrs(attrs.attributes)

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(codeBlock: body))
      }
    }

    return decl
  }

  func generate(initializerDecl node: InitializerDeclSyntax) -> BridgedConstructorDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)

    let decl = BridgedConstructorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      initKeywordLoc: self.generateSourceLoc(node.initKeyword),
      failabilityMarkLoc: self.generateSourceLoc(node.optionalMark),
      isIUO: node.optionalMark?.rawTokenKind == .exclamationMark,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: self.generate(functionParameterClause: node.signature.parameterClause, forSubscript: false),
      asyncSpecifierLoc: self.generateSourceLoc(node.signature.effectSpecifiers?.asyncSpecifier),
      throwsSpecifierLoc: self.generateSourceLoc(node.signature.effectSpecifiers?.throwsClause?.throwsSpecifier),
      thrownType: self.generate(type: node.signature.effectSpecifiers?.thrownError),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.setAttrs(attrs.attributes)

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(codeBlock: body))
      }
    }

    return decl
  }

  func generate(deinitializerDecl node: DeinitializerDeclSyntax) -> BridgedDestructorDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)

    let decl = BridgedDestructorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      deinitKeywordLoc: self.generateSourceLoc(node.deinitKeyword)
    )
    decl.asDecl.setAttrs(attrs.attributes)

    if let body = node.body {
      self.withDeclContext(decl.asDeclContext) {
        decl.setParsedBody(self.generate(codeBlock: body))
      }
    }

    return decl
  }
}

// MARK: - OperatorDecl

extension BridgedOperatorFixity {
  fileprivate init?(from keyword: Keyword?) {
    switch keyword {
    case .infix: self = .infix
    case .prefix: self = .prefix
    case .postfix: self = .postfix
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func generate(operatorDecl node: OperatorDeclSyntax) -> BridgedOperatorDecl {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)
    let (precedenceGroupName, precedenceGroupLoc) =
      self.generateIdentifierAndSourceLoc(node.operatorPrecedenceAndTypes?.precedenceGroup)

    let fixity: BridgedOperatorFixity
    if let value = BridgedOperatorFixity(from: node.fixitySpecifier.keywordKind) {
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
      operatorKeywordLoc: self.generateSourceLoc(node.operatorKeyword),
      name: name,
      nameLoc: nameLoc,
      colonLoc: self.generateSourceLoc(node.operatorPrecedenceAndTypes?.colon),
      precedenceGroupName: precedenceGroupName,
      precedenceGroupLoc: precedenceGroupLoc
    )
  }
}

// MARK: - PrecedenceGroupDecl

extension BridgedAssociativity {
  fileprivate init?(from keyword: Keyword?) {
    switch keyword {
    case .none?: self = .none
    case .left?: self = .left
    case .right?: self = .right
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func generate(precedenceGroupDecl node: PrecedenceGroupDeclSyntax) -> BridgedPrecedenceGroupDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

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
        switch keyword.keywordKind {
        case .higherThan:
          if let current = body.higherThanRelation {
            diagnoseDuplicateSyntax(relation, original: current)
          } else {
            body.higherThanRelation = relation
          }
        case .lowerThan:
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
#if RESILIENT_SWIFT_SYNTAX
      @unknown default:
        fatalError()
#endif
      }
    }

    let associativityValue: BridgedAssociativity
    if let token = body.associativity?.value {
      if let value = BridgedAssociativity(from: token.keywordKind) {
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
      if token.keywordKind == .true {
        assignmentValue = true
      } else {
        self.diagnose(Diagnostic(node: token, message: UnexpectedTokenKindError(token: token)))
        assignmentValue = false
      }
    } else {
      assignmentValue = false
    }

    let decl = BridgedPrecedenceGroupDecl.createParsed(
      declContext: self.declContext,
      precedencegroupKeywordLoc: self.generateSourceLoc(node.precedencegroupKeyword),
      name: name,
      nameLoc: nameLoc,
      leftBraceLoc: self.generateSourceLoc(node.leftBrace),
      associativityLabelLoc: self.generateSourceLoc(body.associativity?.associativityLabel),
      associativityValueLoc: self.generateSourceLoc(body.associativity?.value),
      associativity: associativityValue,
      assignmentLabelLoc: self.generateSourceLoc(body.assignment?.assignmentLabel),
      assignmentValueLoc: self.generateSourceLoc((body.assignment?.value)),
      isAssignment: assignmentValue,
      higherThanKeywordLoc: self.generateSourceLoc((body.higherThanRelation?.higherThanOrLowerThanLabel)),
      higherThanNames: self.generate(precedenceGroupNameList: body.higherThanRelation?.precedenceGroups),
      lowerThanKeywordLoc: self.generateSourceLoc(body.lowerThanRelation?.higherThanOrLowerThanLabel),
      lowerThanNames: self.generate(precedenceGroupNameList: body.lowerThanRelation?.precedenceGroups),
      rightBraceLoc: self.generateSourceLoc(node.rightBrace)
    )
    decl.asDecl.setAttrs(attrs.attributes)
    return decl
  }
}

// MARK: - ImportDecl

extension BridgedImportKind {
  fileprivate init?(from keyword: Keyword?) {
    switch keyword {
    case .typealias: self = .type
    case .struct: self = .struct
    case .class: self = .class
    case .enum: self = .enum
    case .protocol: self = .protocol
    case .var, .let: self = .var
    case .func: self = .func
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  func generate(importDecl node: ImportDeclSyntax) -> BridgedImportDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let importKind: BridgedImportKind
    if let specifier = node.importKindSpecifier {
      if let value = BridgedImportKind(from: specifier.keywordKind) {
        importKind = value
      } else {
        self.diagnose(Diagnostic(node: specifier, message: UnexpectedTokenKindError(token: specifier)))
        importKind = .module
      }
    } else {
      importKind = .module
    }

    let decl = BridgedImportDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      importKeywordLoc: self.generateSourceLoc(node.importKeyword),
      importKind: importKind,
      importKindLoc: self.generateSourceLoc(node.importKindSpecifier),
      path: node.path.lazy.map {
        self.generateLocatedIdentifier($0.name)
      }.bridgedArray(in: self)
    )
    decl.asDecl.setAttrs(attrs.attributes)
    return decl
  }
}

extension ASTGenVisitor {
  @inline(__always)
  func generate(memberBlockItemList node: MemberBlockItemListSyntax) -> BridgedArrayRef {
    var allBridged: [BridgedDecl] = []
    visitIfConfigElements(node, of: MemberBlockItemSyntax.self) { element in
      if let ifConfigDecl = element.decl.as(IfConfigDeclSyntax.self) {
        return .ifConfigDecl(ifConfigDecl)
      }

      return .underlying(element)
    } body: { member in
      // TODO: Set semicolon loc.
      allBridged.append(self.generate(decl: member.decl))
    }

    return allBridged.lazy.bridgedArray(in: self)
  }

  @inline(__always)
  func generate(inheritedTypeList node: InheritedTypeListSyntax) -> BridgedArrayRef {
    node.lazy.map { self.generate(type: $0.type) }.bridgedArray(in: self)
  }

  @inline(__always)
  func generate(precedenceGroupNameList node: PrecedenceGroupNameListSyntax) -> BridgedArrayRef {
    node.lazy.map {
      self.generateLocatedIdentifier($0.name)
    }.bridgedArray(in: self)
  }
}
