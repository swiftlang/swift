//===--- Decls.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) @_spi(RawSyntax) @_spi(Compiler) import SwiftSyntax

// MARK: - TypeDecl

extension ASTGenVisitor {
  func generate(decl node: DeclSyntax) -> BridgedDecl? {
    switch node.as(DeclSyntaxEnum.self) {
    case .accessorDecl:
      fatalError("Should be generated as a part of another decl")
    case .actorDecl(let node):
      return self.generate(actorDecl: node)?.asDecl
    case .associatedTypeDecl(let node):
      return self.generate(associatedTypeDecl: node)?.asDecl
    case .classDecl(let node):
      return self.generate(classDecl: node)?.asDecl
    case .deinitializerDecl(let node):
      return self.generate(deinitializerDecl: node).asDecl
    case .editorPlaceholderDecl:
      fatalError("EditorPlaceholderDeclSyntax should not be used")
    case .enumCaseDecl(let node):
      let decl = self.generate(enumCaseDecl: node)
      // Only an enum can contain 'case' elements; the C++ parser builds no
      // declaration for one anywhere else.
      guard self.isDirectlyInEnumBody(node) else {
        self.diagnose(.disallowed_enum_element, at: node.caseKeyword)
        return nil
      }
      return decl.asDecl
    case .enumDecl(let node):
      return self.generate(enumDecl: node)?.asDecl
    case .extensionDecl(let node):
      return self.generate(extensionDecl: node).asDecl
    case .fileDefaultDecl(let node):
      return self.generate(fileDefaultDecl: node)?.asDecl
    case .functionDecl(let node):
      return self.generate(functionDecl: node)?.asDecl
    case .ifConfigDecl:
      fatalError("Should have been handled by the caller")
    case .importDecl(let node):
      return self.generate(importDecl: node)?.asDecl
    case .initializerDecl(let node):
      return self.generate(initializerDecl: node).asDecl
    case .macroDecl(let node):
      return self.generate(macroDecl: node)?.asDecl
    case .macroExpansionDecl(let node):
      return self.generate(macroExpansionDecl: node).asDecl
    case .missingDecl(let node):
      return self.generate(missingDecl: node)?.asDecl
    case .operatorDecl(let node):
      return self.generate(operatorDecl: node)?.asDecl
    case .poundSourceLocation(let node):
      // The line table is built elsewhere; only check the directive here.
      self.check(poundSourceLocation: node)
      return nil
    case .precedenceGroupDecl(let node):
      return self.generate(precedenceGroupDecl: node)?.asDecl
    case .protocolDecl(let node):
      return self.generate(protocolDecl: node)?.asDecl
    case .structDecl(let node):
      return self.generate(structDecl: node)?.asDecl
    case .subscriptDecl(let node):
      return self.generate(subscriptDecl: node).asDecl
    case .typeAliasDecl(let node):
      return self.generate(typeAliasDecl: node)?.asDecl
    case .unexpectedCodeDecl:
      // Ignore unexpected code.
      return nil
    case .variableDecl(let node):
      return self.generate(variableDecl: node)
    }
  }

  func generateIdentifierDeclNameAndLoc(_ node: TokenSyntax) -> (identifier: Identifier, sourceLoc: SourceLoc)? {
    guard node.presence == .present else {
      return nil
    }
    let result = self.generateIdentifierAndSourceLoc(node)
    guard result.identifier != nil else {
      return nil
    }
    self.diagnoseDollarDeclName(node)
    return result
  }

  func generate(typeAliasDecl node: TypeAliasDeclSyntax) -> BridgedTypeAliasDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

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
    decl.asDecl.attachParsedAttrs(attrs.attributes)
    return decl
  }

  func generate(enumDecl node: EnumDeclSyntax) -> BridgedEnumDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    let decl = BridgedEnumDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      enumKeywordLoc: self.generateSourceLoc(node.enumKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.asNominalTypeDecl.setParsedMembers($0, fingerprint: $1)
    }

    return decl
  }

  func generate(structDecl node: StructDeclSyntax) -> BridgedNominalTypeDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    let decl = BridgedStructDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      structKeywordLoc: self.generateSourceLoc(node.structKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.setParsedMembers($0, fingerprint: $1)
    }

    return decl
  }

  func generate(classDecl node: ClassDeclSyntax) -> BridgedNominalTypeDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.generateSourceLoc(node.classKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock),
      isActor: false
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.setParsedMembers($0, fingerprint: $1)
    }

    return decl
  }

  func generate(actorDecl node: ActorDeclSyntax) -> BridgedNominalTypeDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    let decl = BridgedClassDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      classKeywordLoc: self.generateSourceLoc(node.actorKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock),
      isActor: true
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.setParsedMembers($0, fingerprint: $1)
    }

    return decl
  }

  func generate(protocolDecl node: ProtocolDeclSyntax) -> BridgedNominalTypeDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }
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
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes, allowClassRequirement: true, allowAnyObject: true),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.setParsedMembers($0, fingerprint: $1)
    }

    return decl
  }

  func generate(associatedTypeDecl node: AssociatedTypeDeclSyntax) -> BridgedAssociatedTypeDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    // An 'associatedtype' is only meaningful in a protocol, and the AST verifier
    // enforces that. The C++ parser reports a bad placement and produces no
    // decl at all, rather than one the verifier will reject.
    guard self.declContext.isProtocolContext else {
      self.diagnose(
        .associatedtype_outside_protocol,
        at: node.associatedtypeKeyword,
        fixIts: [
          .init(
            replace: self.generateCharSourceRange(node.associatedtypeKeyword),
            with: "typealias"
          )
        ]
      )
      return nil
    }

    let decl = BridgedAssociatedTypeDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      associatedtypeKeywordLoc: self.generateSourceLoc(node.associatedtypeKeyword),
      name: name,
      nameLoc: nameLoc,
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes, allowAnyObject: true),
      defaultType: self.generate(type: node.initializer?.value),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)
    return decl
  }
}

// MARK: - ExtensionDecl

extension ASTGenVisitor {
  func generate(extensionDecl node: ExtensionDeclSyntax) -> BridgedExtensionDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    // `extension P.Protocol { }` (a protocol metatype extension) is recognized
    // in the `ExtensionDecl` constructor, which strips the `.Protocol` and
    // marks the extension; ASTGen just forwards the written type.
    let decl = BridgedExtensionDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      extensionKeywordLoc: self.generateSourceLoc(node.extensionKeyword),
      extendedType: self.generate(type: node.extendedType),
      inheritedTypes: self.generate(inheritedTypeList: node.inheritanceClause?.inheritedTypes),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause),
      braceRange: self.generateMemberBraceRange(node.memberBlock)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    self.generateMembers(of: node, decl: decl.asDecl, declContext: decl.asDeclContext) {
      decl.setParsedMembers($0, fingerprint: $1)
    }

    // An extension is only valid at file scope. One that ended up elsewhere,
    // e.g. through parser recovery, is diagnosed and marked invalid so that the
    // type checker leaves it alone.
    if !self.declContext.isModuleScopeContext {
      self.diagnose(.decl_inner_scope, at: node.extensionKeyword)
      decl.asDecl.setInvalid()
    }

    return decl
  }
}

extension ASTGenVisitor {
  /// The brace range of a nominal type or extension body.
  ///
  /// When the parser had to synthesize the braces there is nothing to point at,
  /// and the synthesized tokens sit past the last real token -- on trailing
  /// trivia, or at end of file. That is not safe to use as a range end:
  /// `getCharSourceRangeOfScope` extends a scope's end with
  /// `Lexer::getLocForEndOfToken`, so an end on trivia lexes forward into the
  /// next declaration, and the scope then fails ASTScope's containment check.
  /// Collapse to the last token that *is* present instead, which is what the C++
  /// parser ends up doing for e.g. `extension Foo` with no body.
  func generateMemberBraceRange(_ memberBlock: MemberBlockSyntax) -> SourceRange {
    if memberBlock.leftBrace.presence == .present {
      // An unterminated body still has a '{' and contents to point at; only the
      // end needs care, which `generateRBraceLoc` handles.
      return SourceRange(
        start: self.generateSourceLoc(memberBlock.leftBrace),
        end: self.generateRBraceLoc(memberBlock.rightBrace, of: memberBlock)
      )
    }
    // No '{' at all, as in `extension Foo` with no body.
    //
    // The body must still contain whatever members it holds: swift-syntax's
    // recovery can absorb the declarations that follow into a brace-less member
    // block -- a bare `extension` swallows the extensions after it -- and a range
    // that excluded them would fail the verifier's containment check. Prefer the
    // members' own extent, and collapse only when there are none.
    let contentRange = self.generateSourceRange(memberBlock.members)
    if contentRange.start.isValid {
      return contentRange
    }
    guard let precedingToken = memberBlock.previousToken(viewMode: .sourceAccurate) else {
      return self.generateSourceRange(memberBlock)
    }
    let loc = self.generateSourceLoc(precedingToken)
    return SourceRange(start: loc, end: loc)
  }
}

// MARK: - EnumCaseDecl

extension ASTGenVisitor {
  func generate(enumCaseElement node: EnumCaseElementSyntax) -> BridgedEnumElementDecl? {
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    var rawValue: BridgedExpr? = nil
    if let rawValueNode = node.rawValue?.value {
      rawValue = self.generate(expr: rawValueNode)

      // Without `LiteralExpressions`, a raw value has to be one of a fixed set of
      // literal forms. The C++ parser checks the same list, reports the same
      // diagnostic, and then drops the raw value; keeping it would leave an
      // expression Sema is not prepared for on the EnumElementDecl.
      if !ctx.langOpts.hasFeature(.LiteralExpressions),
        !rawValueNode.isValidEnumRawValueLiteral
      {
        self.diagnose(.nonliteral_enum_case_raw_value, at: rawValueNode)
        rawValue = nil
      }
    }

    return .createParsed(
      self.ctx,
      declContext: self.declContext,
      name: name,
      nameLoc: nameLoc,
      parameterList: self.generate(enumCaseParameterClause: node.parameterClause),
      equalsLoc: self.generateSourceLoc(node.rawValue?.equal),
      rawValue: rawValue.asNullable
    )
  }

  func generate(enumCaseDecl node: EnumCaseDeclSyntax) -> BridgedEnumCaseDecl {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)

    // All attributes goes to each element.
    //
    // `compactMap(in:)` rather than `lazy.compactMap { }.bridgedArray`: the
    // latter evaluates the transform twice, once to compute `count` and again to
    // fill the buffer, which would build every element -- attaching its
    // attributes, and emitting any diagnostic it produces -- twice.
    let elements = node.elements.compactMap(in: self) { elem -> BridgedEnumElementDecl? in
      guard let elemDecl = self.generate(enumCaseElement: elem) else {
        return nil
      }
      elemDecl.asDecl.attachParsedAttrs(attrs.attributes)
      return elemDecl
    }
    return .createParsed(
      declContext: self.declContext,
      caseKeywordLoc: self.generateSourceLoc(node.caseKeyword),
      elements: elements
    )
  }
}

// MARK: - AbstractStorageDecl

extension ASTGenVisitor {
  func generate(
    accessorSpecifier specifier: TokenSyntax,
    modifiers: DeclModifierListSyntax
  ) -> swift.AccessorKind? {
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
      return .unsafeAddress
    case .unsafeMutableAddress:
      return .unsafeMutableAddress
    case ._read:
      return ._read
    case ._modify:
      return ._modify
    case .`init`:
      return .Init
    case .read:
      self.diagnose(.old_coroutine_accessor("borrow", "read"), at: specifier)
      return .yielding_borrow
    case .modify:
      self.diagnose(.old_coroutine_accessor("mutate", "modify"), at: specifier)
      return .yielding_mutate
    case .borrow:
      if modifiers.first(where: { $0.name.text == "yielding" }) != nil {
        return .yielding_borrow
      }
      precondition(ctx.langOpts.hasFeature(.BorrowAndMutateAccessors), "(compiler bug) 'borrow' accessor should only be parsed with 'BorrowAndMutateAccessors' feature")
      return .borrow
    case .mutate:
      if modifiers.first(where: { $0.name.text == "yielding" }) != nil {
        return .yielding_mutate
      }
      precondition(ctx.langOpts.hasFeature(.BorrowAndMutateAccessors), "(compiler bug) 'mutate' accessor should only be parsed with 'BorrowAndMutateAccessors' feature")
      return .mutate
    default:
      self.diagnose(.unknownAccessorSpecifier(specifier))
      return nil
    }
  }

  private func generate(
    accessorDecl node: AccessorDeclSyntax,
    kind: swift.AccessorKind,
    for storage: BridgedAbstractStorageDecl,
    allowBody: Bool = true
  ) -> BridgedAccessorDecl? {
    var attrs = BridgedDeclAttributes()

    // '@' attributes.
    self.generateDeclAttributes(attributeList: node.attributes) { attr in
      attrs.add(attr)
    }

    // The modifiers.
    //
    // 'yielding' is not a modifier of its own: it is part of the spelling of the
    // 'yielding borrow' and 'yielding mutate' accessor kinds, and is consumed by
    // `generate(accessorSpecifier:modifiers:)` below. Passing it to
    // `generate(declModifier:)` would not find a declaration attribute for it.
    for m in node.modifiers where m.name.keywordKind != .yielding {
      if let g = self.generate(declModifier: m) {
        attrs.add(g)
      }
    }

    // 'yielding borrow' and 'yielding mutate' are gated on a feature flag. The
    // parser accepts them regardless so that the diagnostic is about the
    // feature rather than about the syntax.
    if kind.requiresCoroutineAccessorsFeature,
      !self.ctx.langOpts.hasFeature(.CoroutineAccessors)
    {
      self.diagnose(
        .accessor_requires_coroutine_accessors(String(bridged: kind.nameForDiagnostic)),
        at: node.accessorSpecifier
      )
    }
    // 'borrow' and 'mutate' only make sense in a type.
    if (kind == .borrow || kind == .mutate), !self.declContext.isTypeContext {
      self.diagnose(
        .borrow_mutate_accessor_not_supported_in_decl(
          String(bridged: kind.diagnosticName(article: true, underscored: false))),
        at: node.accessorSpecifier
      )
    }
    // Only a 'get' accessor may be effectful. For any other kind, diagnose each
    // specifier and drop it: an AccessorDecl that is both non-'get' and 'async'
    // violates an AccessorDecl invariant and would trip an assertion.
    var asyncSpecifier = node.effectSpecifiers?.asyncSpecifier
    var throwsClause = node.effectSpecifiers?.throwsClause
    if !kind.allowsEffectsSpecifiers {
      let kindName = String(bridged: kind.name)
      for specifier in [asyncSpecifier, throwsClause?.throwsSpecifier].compactMap({ $0 }) {
        self.diagnose(
          .invalid_accessor_specifier(kindName, String(syntaxText: specifier.rawText)),
          at: specifier
        )
      }
      asyncSpecifier = nil
      throwsClause = nil
    }

    let accessor = BridgedAccessorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      kind: kind,
      storage: storage,
      declLoc: self.generateSourceLoc(node.accessorSpecifier),
      accessorKeywordLoc: self.generateSourceLoc(node.accessorSpecifier),
      parameterList: self.generate(accessorParameters: node.parameters),
      asyncSpecifierLoc: self.generateSourceLoc(asyncSpecifier),
      throwsSpecifierLoc: self.generateSourceLoc(throwsClause),
      thrownType: self.generate(type: throwsClause?.type)
    )
    // With the CoroutineAccessors feature enabled, `_read`/`_modify` are just a
    // spelling of the yielding accessors; rewrite them to match the C++ parser
    // (ParsedAccessors::record).  ASTGen only parses surface source.
    accessor.remapLegacyCoroutineAccessorIfEnabled()
    accessor.asDecl.attachParsedAttrs(attrs)
    if allowBody, let body = node.body {
      self.attachBody(body, to: accessor.asDecl, declContext: accessor.asDeclContext) {
        accessor.setParsedBody($0)
      }
    }
    return accessor
  }

  /// Report a second accessor of a kind that already appeared.
  ///
  /// The C++ parser reports this from `Parser::parseGetSet` and then keeps the
  /// duplicate anyway, so the AST is identical either way. Emitting the error is
  /// nonetheless load-bearing: `AbstractStorageDecl::getAccessor` returns only
  /// the first accessor of a kind, so the AST verifier's
  /// "storage declaration has different accessor for this kind" check fails on
  /// the second one -- and that check only runs when no error has been emitted.
  private func diagnoseDuplicateAccessor(
    _ node: AccessorDeclSyntax,
    kind: swift.AccessorKind,
    previous: AccessorDeclSyntax,
    isSubscript: Bool
  ) {
    let underscored = self.ctx.langOpts.hasFeature(.CoroutineAccessors)
    self.diagnose(
      .duplicate_accessor(
        isSubscript ? 1 : 0,
        String(bridged: kind.diagnosticName(article: true, underscored: underscored))
      ),
      at: node.accessorSpecifier
    )
    self.diagnose(
      .previous_accessor(
        String(bridged: kind.diagnosticName(article: false, underscored: underscored)),
        true
      ),
      at: previous.accessorSpecifier
    )
  }

  /// An accessor that was generated, with the syntax it came from.
  private struct ParsedAccessor {
    let kind: swift.AccessorKind
    let node: AccessorDeclSyntax
    let decl: BridgedAccessorDecl
  }

  /// Diagnose accessors that cannot be provided together, and storage that has
  /// a writing accessor but nothing to read through.
  ///
  /// This is `Parser::ParsedAccessors::classify`.
  private func classifyAccessors(
    _ accessors: [ParsedAccessor],
    isSubscript: Bool,
    braceLoc: SourceLoc,
    isInvalid: Bool,
    storageSyntax: Syntax?
  ) {
    // If there was a problem parsing the accessors, mark them all invalid to
    // avoid tripping later invariants, and don't diagnose what is missing.
    if isInvalid {
      for accessor in accessors {
        accessor.decl.asDecl.setInvalid()
      }
    }

    // The first accessor of each kind is the one that counts.
    var first: [swift.AccessorKind: ParsedAccessor] = [:]
    for accessor in accessors where first[accessor.kind] == nil {
      first[accessor.kind] = accessor
    }
    func accessor(_ kind: swift.AccessorKind) -> ParsedAccessor? { first[kind] }

    let coroutineAccessors = self.ctx.langOpts.hasFeature(.CoroutineAccessors)
    func name(_ kind: swift.AccessorKind, article: Bool, legacy: Bool) -> String {
      String(bridged: kind.diagnosticName(article: article, underscored: legacy))
    }
    /// The legacy spelling that corresponds to a yielding accessor.
    func legacyKind(of kind: swift.AccessorKind) -> swift.AccessorKind? {
      switch kind {
      case .yielding_borrow: return ._read
      case .yielding_mutate: return ._modify
      default: return nil
      }
    }
    func diagnoseConflict(_ firstKind: swift.AccessorKind, _ secondKind: swift.AccessorKind) {
      guard let firstAccessor = accessor(firstKind), let secondAccessor = accessor(secondKind) else {
        return
      }
      let legacy =
        legacyKind(of: firstKind) == secondKind || legacyKind(of: secondKind) == firstKind
        || coroutineAccessors
      self.diagnose(
        .conflicting_accessor(
          isSubscript ? 1 : 0,
          name(secondKind, article: true, legacy: legacy),
          name(firstKind, article: true, legacy: legacy)
        ),
        at: secondAccessor.node.accessorSpecifier
      )
      self.diagnose(
        .previous_accessor(name(firstKind, article: false, legacy: legacy), false),
        at: firstAccessor.node.accessorSpecifier
      )
      secondAccessor.decl.asDecl.setInvalid()
    }

    // The observing accessors are not allowed on subscripts.
    if isSubscript {
      for (kind, index) in [(swift.AccessorKind.willSet, UInt(0)), (.didSet, UInt(1))] {
        if let observer = accessor(kind) {
          self.diagnose(.observing_accessor_in_subscript(index), at: observer.node.accessorSpecifier)
          observer.decl.asDecl.setInvalid()
        }
      }
    }

    // 'get', '_read', 'read' and a non-mutable addressor are all exclusive.
    if accessor(.get) != nil {
      diagnoseConflict(.get, ._read)
      diagnoseConflict(.get, .yielding_borrow)
      diagnoseConflict(.get, .unsafeAddress)
      diagnoseConflict(.get, .borrow)
    } else if accessor(._read) != nil {
      diagnoseConflict(._read, .yielding_borrow)
      diagnoseConflict(._read, .unsafeAddress)
      diagnoseConflict(._read, .borrow)
    } else if accessor(.yielding_borrow) != nil {
      diagnoseConflict(.yielding_borrow, .unsafeAddress)
      diagnoseConflict(.yielding_borrow, .borrow)
    } else if accessor(.unsafeAddress) != nil {
      // Nothing to check; the C++ parser compares 'yielding borrow' with
      // 'borrow' here, which can never both exist in this branch.
    } else if accessor(.borrow) != nil {
      // Nothing can go wrong.
    } else if let mutator = [.Init, .set, ._modify, .yielding_mutate, .unsafeMutableAddress, .mutate]
      .lazy.compactMap({ accessor($0) }).first
    {
      // A writing accessor needs a reading one.
      if !isInvalid {
        let diagnostic: CompilerDiagnostic
        let mutatorName = name(
          mutator.kind, article: true, legacy: mutator.kind == ._modify || coroutineAccessors)
        if accessor(.unsafeMutableAddress) != nil || accessor(._modify) != nil
          || accessor(.yielding_mutate) != nil
        {
          diagnostic = .missing_reading_accessor(isSubscript ? 1 : 0, mutatorName)
        } else if accessor(.mutate) != nil {
          diagnostic = .missing_borrow_accessor(isSubscript ? 1 : 0, mutatorName)
        } else {
          diagnostic = .missing_getter(isSubscript ? 1 : 0, mutatorName)
        }
        self.diagnose(diagnostic, at: mutator.node.accessorSpecifier)
      }
    } else if isSubscript {
      // Subscripts always have to have some sort of accessor.
      if !isInvalid {
        self.diagnose(.subscript_without_get, at: braceLoc)
      }
    }

    // '_modify', 'modify' and 'unsafeMutableAddress' are all mutually exclusive;
    // so are 'mutate' and 'set', and 'unsafeMutableAddress' with 'set'/'mutate'.
    if accessor(.set) != nil {
      diagnoseConflict(.set, .unsafeMutableAddress)
      diagnoseConflict(.set, .mutate)
    } else if accessor(._modify) != nil {
      diagnoseConflict(._modify, .yielding_mutate)
      diagnoseConflict(._modify, .unsafeMutableAddress)
    } else if accessor(.mutate) != nil {
      diagnoseConflict(.mutate, .unsafeMutableAddress)
    }

    if let initAccessor = accessor(.Init) {
      var enclosing: Syntax? = storageSyntax?.parent
      while let current = enclosing, !(current.is(ExtensionDeclSyntax.self) || current.is(StructDeclSyntax.self)
        || current.is(ClassDeclSyntax.self) || current.is(EnumDeclSyntax.self) || current.is(ActorDeclSyntax.self)
        || current.is(ProtocolDeclSyntax.self))
      {
        enclosing = current.parent
      }
      if enclosing?.is(ExtensionDeclSyntax.self) == true {
        self.diagnose(.init_accessor_is_not_in_the_primary_declaration, at: initAccessor.node.accessorSpecifier)
      } else if !self.declContext.isTypeContext || isSubscript {
        self.diagnose(.init_accessor_is_not_on_property, at: initAccessor.node.accessorSpecifier)
      }
    }

    // A 'borrow' can only be paired with 'mutate'.
    if let borrow = accessor(.borrow),
      accessor(.set) != nil || accessor(._modify) != nil || accessor(.yielding_mutate) != nil
        || accessor(.unsafeMutableAddress) != nil
    {
      self.diagnose(.borrow_mutate_pair_restrict("borrow", "mutate"), at: borrow.node.accessorSpecifier)
    }
    if let mutate = accessor(.mutate),
      accessor(.get) != nil || accessor(._read) != nil || accessor(.yielding_borrow) != nil
        || accessor(.unsafeAddress) != nil
    {
      self.diagnose(.borrow_mutate_pair_restrict("mutate", "borrow"), at: mutate.node.accessorSpecifier)
    }
  }

  private func generate(
    accessorBlock: AccessorBlockSyntax,
    for storage: BridgedAbstractStorageDecl
  ) -> BridgedAccessorRecord {
    let leftBrace = self.generateSourceLoc(accessorBlock.leftBrace)

    // A '}' the parser synthesized during recovery still has a position: a
    // zero-width one past the previous token *and its trailing trivia*, which
    // for an unterminated accessor block lands inside the following
    // declaration. The resulting storage declaration then overlaps its
    // successor, which the availability-scope and ASTScope builders reject. Use
    // the last token actually present instead, matching the C++ parser, which
    // ends the range at the last token it consumed.
    var rightBrace = self.generateSourceLocIfPresent(accessorBlock.rightBrace)
    if rightBrace == nil {
      rightBrace = self.generateSourceLoc(
        accessorBlock.lastToken(viewMode: .sourceAccurate) ?? accessorBlock.leftBrace
      )
    }

    // Whether the storage is a protocol requirement.
    var inProtocol = false
    for ancestor in sequence(first: accessorBlock.parent, next: { $0?.parent }) {
      if let ancestor, ancestor.is(VariableDeclSyntax.self) || ancestor.is(SubscriptDeclSyntax.self) {
        inProtocol = self.isProtocolMember(ancestor)
        break
      }
    }

    // The duplicate check below, and `classifyAccessors` after it, are handled
    // here because omitting them lets an invalid AST reach the verifier; see
    // `diagnoseDuplicateAccessor`.
    switch accessorBlock.accessors {
    case .accessors(let accessors):
      /// The first accessor seen of each kind, for duplicate detection.
      var firstOfKind: [swift.AccessorKind: AccessorDeclSyntax] = [:]
      /// Every accessor that was generated, in order, for `classify`.
      var generated: [ParsedAccessor] = []
      /// Whether a 'get' that is 'async' or 'throws' has been seen, and the
      /// accessors other than 'get' seen so far. An effectful 'get' cannot
      /// coexist with any other accessor.
      var hasEffectfulGet = false
      var nonGetSoFar: [(kind: swift.AccessorKind, node: AccessorDeclSyntax)] = []
      let isSubscript = accessorBlock.parent?.is(SubscriptDeclSyntax.self) == true
      /// Set once an accessor in a protocol was diagnosed as having a body.
      /// The C++ parser stops at the body it did not skip, so the accessors
      /// after it are dropped and the declaration is invalid.
      var protocolBodyDiagnosed = false

      let record = BridgedAccessorRecord(
        lBraceLoc: leftBrace,
        // Use `compactMap(in:)` rather than `lazy.compactMap { }.bridgedArray`:
        // the latter evaluates the transform twice, once to compute `count` and
        // again to fill the buffer, which would generate every accessor -- and
        // emit any diagnostic it produces -- twice.
        accessors: accessors.compactMap(in: self) { node -> BridgedAccessorDecl? in
          // The kind is computed once here, not again inside
          // `generate(accessorDecl:kind:for:)`: the mapper diagnoses an unknown
          // specifier and a deprecated 'read'/'modify' spelling, so calling it
          // twice would emit those twice.
          //
          // TODO: We could potentially recover from an unknown specifier if this
          // is the first accessor, by treating it as an implicit getter.
          if protocolBodyDiagnosed {
            return nil
          }
          guard let kind = self.generate(accessorSpecifier: node.accessorSpecifier,
                                         modifiers: node.modifiers) else {
            return nil
          }
          // A protocol requirement allows only some accessors, and they have no
          // body or setter parameter name.
          var allowBody = true
          if inProtocol {
            switch kind {
            case .get, .set, .yielding_borrow, .borrow, .mutate, .yielding_mutate:
              break
            default:
              self.diagnose(.expected_getreadborrowsetmutate_in_protocol, at: node.accessorSpecifier)
              return nil
            }
            if node.parameters != nil {
              self.diagnose(.protocol_setter_name, at: node.accessorSpecifier)
            }
            if let body = node.body {
              self.diagnose(
                .unexpected_getset_implementation_in_protocol(
                  String(bridged: kind.diagnosticName(article: false, underscored: false))),
                at: body.leftBrace
              )
              // The C++ parser does not skip the body, so it then fails to find
              // the next accessor at the brace.
              self.diagnose(.expected_getreadborrowsetmutate_in_protocol, at: body.leftBrace)
              protocolBodyDiagnosed = true
              allowBody = false
            }
          }
          if let previous = firstOfKind[kind] {
            self.diagnoseDuplicateAccessor(
              node,
              kind: kind,
              previous: previous,
              isSubscript: accessorBlock.parent?.is(SubscriptDeclSyntax.self) == true
            )
          } else {
            firstOfKind[kind] = node
          }
          if kind == .get {
            let effects = node.effectSpecifiers
            if effects?.asyncSpecifier != nil || effects?.throwsClause != nil {
              if !hasEffectfulGet {
                for previous in nonGetSoFar {
                  self.diagnose(
                    .invalid_accessor_with_effectful_get(String(bridged: previous.kind.name)),
                    at: previous.node.accessorSpecifier
                  )
                }
              }
              hasEffectfulGet = true
            }
          } else {
            nonGetSoFar.append((kind, node))
            if hasEffectfulGet {
              self.diagnose(
                .invalid_accessor_with_effectful_get(String(bridged: kind.name)),
                at: node.accessorSpecifier
              )
            }
          }
          guard let decl = self.generate(accessorDecl: node, kind: kind, for: storage, allowBody: allowBody) else {
            return nil
          }
          generated.append(ParsedAccessor(kind: kind, node: node, decl: decl))
          return decl
        },
        rBraceLoc: rightBrace
      )
      self.classifyAccessors(
        generated,
        isSubscript: isSubscript,
        braceLoc: leftBrace,
        isInvalid: accessorBlock.hasError || self.accessorsAreInvalid || protocolBodyDiagnosed,
        storageSyntax: accessorBlock.parent
      )
      return record
    case .getter(let codeBlock):
      // A protocol requirement lists its accessors; it cannot have an implicit
      // getter. The C++ parser diagnoses the first token and builds no accessor,
      // leaving the type checker to ask for an explicit '{ get }'.
      if inProtocol, let first = codeBlock.first, !accessorBlock.hasError {
        self.diagnose(.expected_getreadborrowsetmutate_in_protocol, at: first)
        return BridgedAccessorRecord(
          lBraceLoc: leftBrace,
          accessors: .init(),
          rBraceLoc: rightBrace
        )
      }
      // `subscript(i: Int) -> Int {}` in a protocol: an empty body is an empty
      // accessor list, and a subscript requirement needs a getter. (Elsewhere it
      // is an empty getter body.)
      if codeBlock.isEmpty, let subscriptDecl = accessorBlock.parent?.as(SubscriptDeclSyntax.self),
        self.isProtocolMember(subscriptDecl), !accessorBlock.hasError
      {
        self.diagnose(.subscript_without_get, at: leftBrace)
      }
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
      if self.accessorsAreInvalid {
        accessor.asDecl.setInvalid()
      }
      self.attachImplicitGetterBody(
        accessorBlock, items: codeBlock, leftBrace: leftBrace, rightBrace: rightBrace, to: accessor)
      return BridgedAccessorRecord(
        lBraceLoc: leftBrace,
        accessors: CollectionOfOne(accessor).bridgedArray(in: self),
        rBraceLoc: rightBrace
      )
    }
  }

  func generate(patternBinding binding: PatternBindingSyntax, attrs: DeclAttributesResult, isAsyncLet: Bool, topLevelDecl: BridgedTopLevelCodeDecl?) -> BridgedPatternBindingEntry? {
    if binding.pattern.is(MissingPatternSyntax.self) {
      // The availability checker requires declarations to have valid SourceLocs, but MissingPatternSyntax lowers to
      // an implicit AnyPattern with invalid SourceLocs. A top-level MissingPatternSyntax could therefore cause us to
      // construct an invalid AST. Drop the whole binding instead.
      // No need to diagnose; SwiftParser should have already diagnosed the `MissingPatternSyntax`.
      return nil
    }

    // A computed property needs a type: there is no initializer to infer it from.
    var missingType = false
    if binding.typeAnnotation == nil, let identifier = binding.pattern.as(IdentifierPatternSyntax.self),
      let accessorBlock = binding.accessorBlock
    {
      var hasReadOrWriteAccessor = false
      switch accessorBlock.accessors {
      case .getter:
        hasReadOrWriteAccessor = true
      case .accessors(let accessors):
        hasReadOrWriteAccessor = accessors.contains {
          switch $0.accessorSpecifier.keywordKind {
          case .get, .set, .unsafeAddress, .unsafeMutableAddress: return true
          default: return false
          }
        }
      }
      if hasReadOrWriteAccessor {
        missingType = true
        self.diagnose(
          .computed_property_missing_type,
          at: identifier,
          fixIts: [
            .init(
              replace: CharSourceRange(
                start: SourceLoc(at: identifier.endPositionBeforeTrailingTrivia, in: self.base),
                byteLength: 0),
              with: ": <# Type #>")
          ]
        )
      }
    }

    let pattern = generate(pattern: binding.pattern)
    if isAsyncLet {
      // `async let _ = ...` has no VarDecl to carry the `async` attribute, so
      // the wildcard itself records it.
      pattern.markWildcardsAsAsyncLet()
    }

    let equalLoc = generateSourceLoc(binding.initializer?.equal)

    var initExpr: BridgedExpr?
    var initContext: BridgedPatternBindingInitializer?
    // The C++ parser gives up on the entry after dropping a disallowed
    // initializer, so any accessors that follow are not parsed.
    var initializerDisallowed = false
    if let initializer = binding.initializer, let variable = binding.parent?.parent,
      self.isProtocolMember(variable)
    {
      // A protocol requirement has no initial value. The C++ parser diagnoses
      // the initializer and drops it.
      self.diagnose(.disallowed_init, at: initializer.equal)
      initializerDisallowed = true
    } else if let initializer = binding.initializer {
      // Create a PatternBindingInitializer if we're not in a local context (this
      // ensures that property initializers are correctly treated as being in a
      // local context).
      if !self.declContext.isLocalContext, topLevelDecl == nil {
        initContext = .create(declContext: self.declContext)
      }
      initExpr = withDeclContext(topLevelDecl?.asDeclContext ?? initContext?.asDeclContext ?? self.declContext) {
        generate(expr: initializer.value)
      }
    }
    if let accessors = binding.accessorBlock, !initializerDisallowed {
      self.accessorsAreInvalid = missingType
      defer { self.accessorsAreInvalid = false }
      // FIXME: We ought to have a better way of converting from the Nullable
      // wrapper back to Optional.
      if let primaryVar = pattern.singleVar.raw.map(BridgedVarDecl.init) {
        let storage = primaryVar.asAbstractStorageDecl
        storage.setAccessors(generate(accessorBlock: accessors, for: storage))

      } else {
        self.diagnose(.getset_nontrivial_pattern, at: binding.pattern)
        self.patternBindingIsInvalid = true
      }
    }
    return BridgedPatternBindingEntry(
      pattern: pattern,
      equalLoc: equalLoc,
      init: initExpr.asNullable,
      initContext: initContext.asNullable
    )
  }

  private func generateBindingEntries(for node: VariableDeclSyntax, attrs: DeclAttributesResult, topLevelDecl: BridgedTopLevelCodeDecl?) -> BridgedArrayRef? {
    var propagatedType: BridgedTypeRepr?
    var entries: [BridgedPatternBindingEntry] = []

    let isAsyncLet =
      node.bindingSpecifier.keywordKind == .let && node.modifiers.contains { $0.name.text == "async" }

    // Generate the bindings in reverse, keeping track of the TypeRepr to
    // propagate to earlier patterns if needed.
    for binding in node.bindings.reversed() {
      guard
        var entry = self.generate(
          patternBinding: binding, attrs: attrs, isAsyncLet: isAsyncLet, topLevelDecl: topLevelDecl)
      else {
        // Missing pattern. Drop this binding.
        continue
      }

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

    if entries.isEmpty {
      // A VariableDeclSyntax is syntactically required to have at least one binding, so this can only happen if all
      // of the bindings had missing patterns.
      return nil
    }

    return entries.reversed().bridgedArray(in: self)
  }

  /// - Parameter isABIOnly: Whether `node` is the counterpart in an `@abi`
  ///   attribute.
  func generate(variableDecl node: VariableDeclSyntax, isABIOnly: Bool = false) -> BridgedDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: true)
    let introducer: BridgedVarDeclIntroducer
    switch node.bindingSpecifier.rawText {
    case "let":
      introducer = .let
    case "var":
      introducer = .var
    case "inout":
      introducer = .inOut
    default:
      // TODO: Diagnostics
      fatalError("invalid pattern binding introducer")
    }

    // @const/@section globals are not top-level, per SE-0492.
    // We follow the same rule for @_extern. The counterpart in an @abi
    // attribute is not executable code, either.
    let isConst = attrs.attributes.hasAttribute(.Section)
      || attrs.attributes.hasAttribute(.ConstVal)
      || attrs.attributes.hasAttribute(.Extern)

    let topLevelDecl: BridgedTopLevelCodeDecl?
    if self.declContext.isModuleScopeContext, self.declContext.parentSourceFile.isScriptMode, !isConst, !isABIOnly,
      !self.suppressTopLevelCode
    {
      topLevelDecl = BridgedTopLevelCodeDecl.create(self.ctx, declContext: self.declContext)
    } else {
      topLevelDecl = nil
    }

    // A declaration nested in an initializer has its own flag.
    let outerBindingIsInvalid = self.patternBindingIsInvalid
    self.patternBindingIsInvalid = false
    let generatedEntries = self.generateBindingEntries(for: node, attrs: attrs, topLevelDecl: topLevelDecl)
    let bindingIsInvalid = self.patternBindingIsInvalid
    self.patternBindingIsInvalid = outerBindingIsInvalid
    guard let entries = generatedEntries else {
      // No bindings with valid SourceLocs. A PatternBindingDecl generated from this would have an invalid SourceRange,
      // violating availability checker invariants. Drop the PBD instead.
      return nil
    }

    // Reject accessors on a 'let'.
    if introducer == .let, !attrs.attributes.hasAttribute(.HasStorage),
      let accessorBlock = node.bindings.first?.accessorBlock
    {
      var observing = false
      var addressed = false
      if case .accessors(let accessors) = accessorBlock.accessors {
        for accessor in accessors {
          switch accessor.accessorSpecifier.keywordKind {
          case .willSet, .didSet: observing = true
          case .unsafeAddress, .unsafeMutableAddress: addressed = true
          default: break
          }
        }
      }
      let diagnostic: CompilerDiagnostic =
        observing
        ? .let_cannot_be_observing_property
        : addressed ? .let_cannot_be_addressed_property : .let_cannot_be_computed_property
      self.diagnose(
        diagnostic,
        at: accessorBlock.leftBrace,
        fixIts: [.init(replace: self.generateCharSourceRange(node.bindingSpecifier), with: "var")]
      )
    }

    let decl = BridgedPatternBindingDecl.createParsed(
      self.ctx,
      declContext: topLevelDecl?.asDeclContext ?? self.declContext,
      attributes: attrs.attributes,
      staticLoc: attrs.staticLoc,
      staticSpelling: attrs.staticSpelling,
      introducerLoc: self.generateSourceLoc(node.bindingSpecifier),
      introducer: introducer,
      entries: entries,
      // Only a declaration that is itself wrapped in a TopLevelCodeDecl is a
      // global; one in a top-level 'do' block is local.
      isTopLevelGlobal: topLevelDecl != nil
    )
    if bindingIsInvalid {
      decl.asDecl.setInvalid()
    }
    if let topLevelDecl {
      let range = self.generateImplicitBraceRange(node)
      let body = BridgedBraceStmt.createImplicit(
        self.ctx,
        lBraceLoc: range.start,
        element: .decl(decl.asDecl),
        rBraceLoc: range.end
      )
      topLevelDecl.setBody(body: body);
      return topLevelDecl.asDecl
    } else {
      return decl.asDecl
    }
  }

  /// Whether `node` is declared directly in a protocol.
  private func isProtocolMember(_ node: some SyntaxProtocol) -> Bool {
    var parent = node.parent
    while let current = parent {
      if let block = current.as(MemberBlockSyntax.self) {
        return block.parent?.is(ProtocolDeclSyntax.self) == true
      }
      guard current.is(MemberBlockItemSyntax.self) || current.is(MemberBlockItemListSyntax.self) else {
        return false
      }
      parent = current.parent
    }
    return false
  }

  /// Diagnose a default argument in a protocol requirement.
  ///
  /// The C++ parser also drops the declaration; ASTGen keeps it, which only
  /// matters for what is diagnosed afterwards.
  private func diagnoseProtocolDefaultArguments(
    in parameters: FunctionParameterListSyntax,
    of node: some SyntaxProtocol,
    at keyword: TokenSyntax,
    _ diagnostic: CompilerDiagnostic
  ) {
    if isProtocolMember(node), parameters.contains(where: { $0.defaultValue != nil }) {
      self.diagnose(diagnostic, at: keyword)
    }
  }

  func generate(subscriptDecl node: SubscriptDeclSyntax) -> BridgedSubscriptDecl {
    let attrs = generateDeclAttributes(node, allowStatic: true)
    self.diagnoseProtocolDefaultArguments(
      in: node.parameterClause.parameters, of: node, at: node.subscriptKeyword,
      .protocol_subscript_argument_init)

    let subscriptDecl = BridgedSubscriptDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      staticLoc: attrs.staticLoc,
      staticSpelling: attrs.staticSpelling,
      subscriptKeywordLoc: self.generateSourceLoc(node.subscriptKeyword),
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: self.generate(functionParameterClause: node.parameterClause, for: .subscript),
      arrowLoc: self.generateSourceLoc(node.returnClause.arrow),
      returnType: self.generate(type: node.returnClause.type),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    subscriptDecl.asDecl.attachParsedAttrs(attrs.attributes)

    if let accessors = node.accessorBlock {
      let storage = subscriptDecl.asAbstractStorageDecl
      storage.setAccessors(generate(accessorBlock: accessors, for: storage))
    } else if !node.hasError,
      !sequence(first: Syntax(node).parent, next: { $0?.parent }).contains(where: { $0?.is(ABIAttributeArgumentsSyntax.self) == true })
    {
      // A subscript needs at least a getter. (One in an '@abi' attribute is
      // only a stub.)
      let next = node.returnClause.lastToken(viewMode: .sourceAccurate)?.nextToken(viewMode: .sourceAccurate)
      let location = next.map(Syntax.init) ?? Syntax(node.returnClause)
      if self.isProtocolMember(node) {
        self.diagnose(
          .expected_lbrace_subscript_protocol,
          at: location,
          fixIts: [
            .init(
              replace: self.generateCharSourceRange(
                start: node.returnClause.endPositionBeforeTrailingTrivia,
                length: SourceLength(utf8Length: 0)
              ),
              with: " { get <#set#> }"
            )
          ]
        )
      } else {
        self.diagnose(.expected_lbrace_subscript, at: location)
      }
    }
    return subscriptDecl
  }

  func generate(accessorBlockFile node: AccessorBlockFileSyntax, for storage: BridgedAbstractStorageDecl) -> [BridgedAccessorDecl] {
    var accessors: [BridgedAccessorDecl] = []
    var firstOfKind: [swift.AccessorKind: AccessorDeclSyntax] = [:]
    for elem in node.accessors {
      guard let kind = self.generate(accessorSpecifier: elem.accessorSpecifier,
                                     modifiers: elem.modifiers) else {
        continue
      }
      if let previous = firstOfKind[kind] {
        self.diagnoseDuplicateAccessor(elem, kind: kind, previous: previous, isSubscript: false)
      } else {
        firstOfKind[kind] = elem
        // An accessor the storage was already written with is the first of its
        // kind, and what a macro-provided one duplicates.
        let existing = storage.accessorLoc(kind)
        if existing.isValid {
          let underscored = self.ctx.langOpts.hasFeature(.CoroutineAccessors)
          self.diagnose(
            .duplicate_accessor(
              0,
              String(bridged: kind.diagnosticName(article: true, underscored: underscored))
            ),
            at: elem.accessorSpecifier
          )
          self.diagnose(
            .previous_accessor(
              String(bridged: kind.diagnosticName(article: false, underscored: underscored)),
              true
            ),
            at: existing
          )
        }
      }
      if let accessor = self.generate(accessorDecl: elem, kind: kind, for: storage) {
        accessors.append(accessor)
      }
    }
    // NOTE: Do not set brace locations even if exist. AST doesn't expect that.
    let record = BridgedAccessorRecord(
      lBraceLoc: nil,
      accessors: accessors.lazy.bridgedArray(in: self),
      rBraceLoc: nil
    )
    // FIXME: The caller should setAccessors() after ASTGen just return parsed accessors.
    storage.setAccessors(record)
    return accessors
  }
}

// MARK: - AbstractFunctionDecl

extension ASTGenVisitor {
  struct GeneratedFunctionSignature {
    var parameterList: BridgedParameterList
    var asyncLoc: SourceLoc
    var isReasync: Bool
    var throwsLoc: SourceLoc
    var isRethrows: Bool
    var thrownType: BridgedTypeRepr?
    var yieldList: BridgedYieldList?
    var returnType: BridgedTypeRepr?
  }
  
  func generate(
    functionSignature node: FunctionSignatureSyntax,
    for context: ParameterContext
  ) -> GeneratedFunctionSignature {
    let parameterList = self.generate(functionParameterClause: node.parameterClause, for: context)
    let asyncLoc = self.generateSourceLoc(node.effectSpecifiers?.asyncSpecifier)
    let isReasync = node.effectSpecifiers?.asyncSpecifier?.rawText == "reasync"
    let throwsLoc = self.generateSourceLoc(node.effectSpecifiers?.throwsClause?.throwsSpecifier)
    let isRethrows = node.effectSpecifiers?.throwsClause?.throwsSpecifier.rawText == "rethrows"
    let thrownType = (node.effectSpecifiers?.thrownError).map(self.generate(type:))
    let returnType = (node.returnClause?.type).map(self.generate(type:))
    let yieldList = self.generate(functionYieldClause: node.yieldClause)
    return GeneratedFunctionSignature(
      parameterList: parameterList,
      asyncLoc: asyncLoc,
      isReasync: isReasync,
      throwsLoc: throwsLoc,
      isRethrows: isRethrows,
      thrownType: thrownType,
      yieldList: yieldList,
      returnType: returnType
    )
  } 
  
  func generate(functionDecl node: FunctionDeclSyntax) -> BridgedFuncDecl? {
    var attrs = self.generateDeclAttributes(node, allowStatic: true)

    // A synthesized 'func' keyword means the parser invented this declaration
    // while recovering -- `associatedtype X<T>` produces one for the `<T>` -- and
    // has already diagnosed it. Everything about such a declaration is
    // synthesized, so its source range is not trustworthy, and building it gives
    // ASTScope a child that does not sit inside its parent.
    guard node.funcKeyword.presence == .present else {
      return nil
    }
    self.diagnoseProtocolDefaultArguments(
      in: node.signature.parameterClause.parameters, of: node, at: node.funcKeyword,
      .protocol_method_argument_init)

    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }
    let signature = self.generate(
      functionSignature: node.signature,
      for: name.isOperator() ? .operator : .function
    )

    let decl = BridgedFuncDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      staticLoc: attrs.staticLoc,
      staticSpelling: attrs.staticSpelling,
      funcKeywordLoc: self.generateSourceLoc(node.funcKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: signature.parameterList,
      asyncSpecifierLoc: signature.asyncLoc.isValid ? signature.asyncLoc : self.generateSourceLoc(node.modifiers.first(where: { $0.name.text == "async" })),
      throwsSpecifierLoc: signature.throwsLoc,
      thrownType: signature.thrownType.asNullable,
      yieldList: signature.yieldList.asNullable,
      returnType: signature.returnType.asNullable,
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    if signature.isReasync {
      attrs.attributes.add(BridgedDeclAttribute.createSimple(self.ctx, kind: .Reasync, atLoc: nil, nameLoc: signature.asyncLoc))
    }
    if signature.isRethrows {
      attrs.attributes.add(BridgedDeclAttribute.createSimple(self.ctx, kind: .Rethrows, atLoc: nil, nameLoc: signature.throwsLoc))
    }
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    if let body = node.body {
      if self.isProtocolMember(node) {
        // A protocol requirement has no body. The C++ parser diagnoses the
        // body and skips it.
        self.diagnose(.protocol_method_with_body, at: body.leftBrace)
      } else {
        self.attachBody(body, to: decl.asDecl, declContext: decl.asDeclContext) {
          decl.setParsedBody($0)
        }
      }
    }

    return decl
  }

  func generate(initializerDecl node: InitializerDeclSyntax) -> BridgedConstructorDecl {
    var attrs = self.generateDeclAttributes(node, allowStatic: false)
    self.diagnoseProtocolDefaultArguments(
      in: node.signature.parameterClause.parameters, of: node, at: node.initKeyword,
      .protocol_init_argument_init)
    // Initializers only exist in types. The C++ parser points at the token after
    // `init` and tells the type checker not to touch the declaration.
    let constructorsNotAllowed = !self.declContext.isTypeContext
    if constructorsNotAllowed {
      self.diagnose(
        .initializer_decl_wrong_scope,
        at: node.initKeyword.nextToken(viewMode: .sourceAccurate).map(Syntax.init) ?? Syntax(node.initKeyword)
      )
    }
    let signature = self.generate(
      functionSignature: node.signature,
      for: .initializer
    )

    let decl = BridgedConstructorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      initKeywordLoc: self.generateSourceLoc(node.initKeyword),
      failabilityMarkLoc: self.generateSourceLoc(node.optionalMark),
      isIUO: node.optionalMark?.rawTokenKind == .exclamationMark,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      parameterList: signature.parameterList,
      asyncSpecifierLoc: signature.asyncLoc.isValid ? signature.asyncLoc : self.generateSourceLoc(node.modifiers.first(where: { $0.name.text == "async" })),
      throwsSpecifierLoc: signature.throwsLoc,
      thrownType: signature.thrownType.asNullable,
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    if signature.isReasync {
      attrs.attributes.add(BridgedDeclAttribute.createSimple(self.ctx, kind: .Reasync, atLoc: nil, nameLoc: signature.asyncLoc))
    }
    if signature.isRethrows {
      attrs.attributes.add(BridgedDeclAttribute.createSimple(self.ctx, kind: .Rethrows, atLoc: nil, nameLoc: signature.throwsLoc))
    }
    decl.asDecl.attachParsedAttrs(attrs.attributes)
    if constructorsNotAllowed {
      decl.asDecl.setInvalid()
    }

    if let returnClause = node.signature.returnClause {
      // `init() -> T`. The result type is ignored -- an initializer's result is
      // implicit -- but it has to be reported, or this is accepted where the C++
      // parser rejects it.
      self.diagnose(.initializer_result_type, at: returnClause.type)
    }

    if let body = node.body {
      if self.isProtocolMember(node) {
        // A protocol requirement has no body. The C++ parser diagnoses the
        // body and skips it.
        self.diagnose(.protocol_init_with_body, at: body.leftBrace)
      } else {
        self.attachBody(body, to: decl.asDecl, declContext: decl.asDeclContext) {
          decl.setParsedBody($0)
        }
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
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    // A 'deinit' is only allowed in a struct, enum, or class, or in an
    // '@_objcImplementation' extension. The C++ parser reports a bad placement
    // and marks the decl invalid so the type checker leaves it alone; without
    // that, Sema dereferences a null nominal type.
    if !self.declContext.allowsDestructorDecl {
      self.diagnose(
        .destructor_decl_outside_class_or_noncopyable,
        at: node.deinitKeyword
      )
      decl.asDecl.setInvalid()
    }

    if let body = node.body {
      self.attachBody(body, to: decl.asDecl, declContext: decl.asDeclContext) {
        decl.setParsedBody($0)
      }
    }

    return decl
  }

  func generate(missingDecl node: MissingDeclSyntax) -> BridgedMissingDecl? {
    // Generate the attributes for diagnostics, but discard the result.
    // There's no use of the attributes in AST at this point.
    // FIXME:  We probably should place 'swift::MissingDecl' with the attributes attached in AST for better IDE experience in custom attributes.
    _ = self.generateDeclAttributes(node, allowStatic: true)
    return nil
  }
}

extension ASTGenVisitor {
  func generate(macroDecl node: MacroDeclSyntax) -> BridgedMacroDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }
    let decl = BridgedMacroDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      macroKeywordLoc: self.generateSourceLoc(node.macroKeyword),
      name: name,
      nameLoc: nameLoc,
      genericParamList: self.generate(genericParameterClause: node.genericParameterClause),
      paramList: self.generate(functionParameterClause: node.signature.parameterClause, for: .macro),
      arrowLoc: self.generateSourceLoc(node.signature.returnClause?.arrow),
      resultType: self.generate(type: node.signature.returnClause?.type),
      definition: self.generate(expr: node.definition?.value),
      genericWhereClause: self.generate(genericWhereClause: node.genericWhereClause)
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    return decl;
  }
}

// MARK: - MacroExpansionDecl

extension ASTGenVisitor {
  func generate(macroExpansionDecl node: MacroExpansionDeclSyntax) -> BridgedMacroExpansionDecl {
    switch self.maybeGenerateBuiltinPound(macroExpansionDecl: node) {
    case .generated(_):
      fatalError("(compiler bug) builtin pound keywords should be handled elsewhere")
    case .ignored:
      // Fallback to MacroExpansionDecl.
      break
    }

    let attrs = self.generateDeclAttributes(node, allowStatic: true)
    let info = self.generate(freestandingMacroExpansion: node)
    let decl = BridgedMacroExpansionDecl.createParsed(
      self.declContext,
      poundLoc: info.poundLoc,
      macroNameRef: info.macroNameRef,
      macroNameLoc: info.macroNameLoc,
      leftAngleLoc: info.leftAngleLoc,
      genericArgs: info.genericArgs,
      rightAngleLoc: info.rightAngleLoc,
      args: info.arguments
    )
    decl.asDecl.attachParsedAttrs(attrs.attributes)

    return decl
  }

  func generateMacroExpansionDecl(macroExpansionExpr node: MacroExpansionExprSyntax) -> BridgedMacroExpansionDecl {
    let info = self.generate(freestandingMacroExpansion: node)
    return .createParsed(
      self.declContext,
      poundLoc: info.poundLoc,
      macroNameRef: info.macroNameRef,
      macroNameLoc: info.macroNameLoc,
      leftAngleLoc: info.leftAngleLoc,
      genericArgs: info.genericArgs,
      rightAngleLoc: info.rightAngleLoc,
      args: info.arguments
    )
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
  func generate(operatorDecl node: OperatorDeclSyntax) -> BridgedOperatorDecl? {
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    let fixity: BridgedOperatorFixity
    if let value = BridgedOperatorFixity(from: node.fixitySpecifier.keywordKind) {
      fixity = value
    } else {
      fixity = .infix
      self.diagnose(.unexpectedTokenKind(token: node.fixitySpecifier))
    }

    // A postfix operator cannot start like the built-in unwrapping operators.
    if fixity == .postfix, node.name.rawText.first == UInt8(ascii: "?") || node.name.rawText.first == UInt8(ascii: "!") {
      self.diagnose(.postfix_operator_name_cannot_start_with_unwrap, at: node.name)
    }

    // Only an infix operator may declare a precedence group. A prefix or postfix
    // operator decl carries no group at all, so passing one through would
    // violate an OperatorDecl invariant.
    var precedenceAndTypes = node.operatorPrecedenceAndTypes
    if fixity != .infix, let group = precedenceAndTypes {
      self.diagnose(.precedencegroup_not_infix, at: group.colon)
      precedenceAndTypes = nil
    }

    let (precedenceGroupName, precedenceGroupLoc) =
      self.generateIdentifierAndSourceLoc(precedenceAndTypes?.precedenceGroup)

    let decl = BridgedOperatorDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      fixity: fixity,
      operatorKeywordLoc: self.generateSourceLoc(node.operatorKeyword),
      name: name,
      nameLoc: nameLoc,
      colonLoc: self.generateSourceLoc(precedenceAndTypes?.colon),
      precedenceGroupName: precedenceGroupName,
      precedenceGroupLoc: precedenceGroupLoc
    )

    // The C++ parser parses the fixity as a declaration modifier, so the
    // declaration carries an `infix`, `prefix` or `postfix` attribute.
    var attrs = BridgedDeclAttributes()
    let fixityKind: swift.DeclAttrKind =
      switch fixity {
      case .infix: .Infix
      case .prefix: .Prefix
      case .postfix: .Postfix
      }
    attrs.add(
      BridgedDeclAttribute.createSimple(
        self.ctx, kind: fixityKind, atLoc: nil, nameLoc: self.generateSourceLoc(node.fixitySpecifier)))
    decl.asDecl.attachParsedAttrs(attrs)
    return decl
  }
}

// MARK: - PrecedenceGroupDecl

extension swift.Associativity {
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
  func generate(precedenceGroupDecl node: PrecedenceGroupDeclSyntax) -> BridgedPrecedenceGroupDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    guard let (name, nameLoc) = self.generateIdentifierDeclNameAndLoc(node.name) else {
      return nil
    }

    struct PrecedenceGroupBody {
      var associativity: PrecedenceGroupAssociativitySyntax? = nil
      var assignment: PrecedenceGroupAssignmentSyntax? = nil
      var higherThanRelation: PrecedenceGroupRelationSyntax? = nil
      var lowerThanRelation: PrecedenceGroupRelationSyntax? = nil
    }

    func diagnoseDuplicateAttribute(_ duplicate: some SyntaxProtocol, name: TokenSyntax) {
      self.diagnose(
        .precedencegroup_attribute_redeclared(String(syntaxText: name.rawText)),
        at: duplicate
      )
    }

    let body = node.groupAttributes.reduce(into: PrecedenceGroupBody()) { body, element in
      switch element {
      case .precedenceGroupRelation(let relation):
        let keyword = relation.higherThanOrLowerThanLabel
        switch keyword.keywordKind {
        case .higherThan:
          if body.higherThanRelation != nil {
            diagnoseDuplicateAttribute(relation, name: keyword)
          } else {
            body.higherThanRelation = relation
          }
        case .lowerThan:
          if body.lowerThanRelation != nil {
            diagnoseDuplicateAttribute(relation, name: keyword)
          } else {
            body.lowerThanRelation = relation
          }
        default:
          return self.diagnose(.unexpectedTokenKind(token: keyword))
        }
      case .precedenceGroupAssignment(let assignment):
        if body.assignment != nil {
          diagnoseDuplicateAttribute(assignment, name: assignment.assignmentLabel)
        } else {
          body.assignment = assignment
        }
      case .precedenceGroupAssociativity(let associativity):
        if body.associativity != nil {
          diagnoseDuplicateAttribute(associativity, name: associativity.associativityLabel)
        } else {
          body.associativity = associativity
        }
      }
    }

    let associativityValue: swift.Associativity
    if let token = body.associativity?.value {
      if let value = swift.Associativity(from: token.keywordKind) {
        associativityValue = value
      } else {
        self.diagnose(.expected_precedencegroup_associativity, at: token)
        associativityValue = .none
      }
    } else {
      associativityValue = .none
    }

    let assignmentValue: Bool
    if let token = body.assignment?.value {
      switch token.keywordKind {
      case .true:
        assignmentValue = true
      case .false:
        assignmentValue = false
      default:
        self.diagnose(.expected_precedencegroup_assignment, at: token)
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
    decl.asDecl.attachParsedAttrs(attrs.attributes)
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
  func generate(importDecl node: ImportDeclSyntax) -> BridgedImportDecl? {
    let attrs = self.generateDeclAttributes(node, allowStatic: false)
    let importKind: BridgedImportKind
    if let specifier = node.importKindSpecifier {
      if let value = BridgedImportKind(from: specifier.keywordKind) {
        importKind = value
      } else {
        self.diagnose(.unexpectedTokenKind(token: specifier))
        importKind = .module
      }
    } else {
      importKind = .module
    }

    // A non-module import needs at least a module name to import *from*, e.g.
    // 'import var Module.member'. The C++ parser reports this and builds no
    // ImportDecl; `ImportDecl::create` asserts the same invariant.
    if importKind != .module, node.path.count == 1, let first = node.path.first {
      self.diagnose(.decl_expected_module_name, at: first.name)
      return nil
    }

    // Only a module alias, not the real name it stands for, may be imported.
    if let first = node.path.first {
      let name = self.generateIdentifier(first.name)
      let aliasRef = self.ctx.aliasForAliasedRealModuleName(name)
      if aliasRef.count != 0 {
        let aliasText = String(decoding: UnsafeBufferPointer(start: aliasRef.data, count: aliasRef.count), as: UTF8.self)
        self.diagnose(
          .expected_module_alias(name, self.ctx.getIdentifier(aliasRef)),
          at: first.name,
          fixIts: [.init(replace: self.generateCharSourceRange(first.name), with: aliasText)]
        )
        return nil
      }
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
    decl.asDecl.attachParsedAttrs(attrs.attributes)
    return decl
  }
}

extension ASTGenVisitor {
  func generate(fileDefaultDecl node: FileDefaultDeclSyntax) -> BridgedFileDefaultDecl? {
    var attrs = BridgedDeclAttributes()
    var addedAny = false

    switch node.specifier {
    case .attribute(let attr):
      self.generateDeclAttribute(attribute: attr) { bridgedAttr in
        attrs.add(bridgedAttr)
        addedAny = true
      }
    case .modifier(let modifier):
      guard case .identifier("nonisolated") = modifier.tokenKind else {
        self.diagnose(.invalidFileDefaultSpecifier(node.specifier))
        return nil
      }
      let nonisolatedAttr = BridgedNonisolatedAttr.createParsed(
        self.ctx,
        atLoc: nil,
        range: self.generateSourceRange(modifier),
        modifier: .none
      )
      attrs.add(nonisolatedAttr.asDeclAttribute)
      addedAny = true
    }
    guard addedAny else {
      self.diagnose(.invalidFileDefaultSpecifier(node.specifier))
      return nil
    }

    return BridgedFileDefaultDecl.createParsed(
      self.ctx,
      declContext: self.declContext,
      defaultKeywordLoc: self.generateSourceLoc(node.defaultKeyword),
      specifiedAttributes: attrs
    )
  }
}

extension ASTGenVisitor {
  func generate(memberBlockItem node: MemberBlockItemSyntax) -> BridgedDecl? {
    if let node = node.decl.as(MacroExpansionDeclSyntax.self) {
      switch self.maybeGenerateBuiltinPound(macroExpansionDecl: node) {
      case .generated(let generated?):
        switch generated.kind {
        case .decl:
          // Actually unreachable as no builtin pound emits a declaration.
          return generated.castToDecl()
        case .stmt, .expr:
          // TODO: Diagnose
          fatalError("builtin pound keyword in declaration member block")
          //return nil
        }
      case .generated(nil):
        return nil
      case .ignored:
        // Fallback to normal macro expansion.
        break
      }
    }
    return self.generate(decl: node.decl)
  }

  @inline(__always)
  func generate(memberBlockItemList node: MemberBlockItemListSyntax) -> [BridgedDecl] {
    var allMembers: [BridgedDecl] = []
    visitIfConfigElements(node, of: MemberBlockItemSyntax.self) { element in
      if let ifConfigDecl = element.decl.as(IfConfigDeclSyntax.self) {
        return .ifConfigDecl(ifConfigDecl)
      }

      return .underlying(element)
    } body: { node in
      guard let member = self.generate(memberBlockItem: node) else {
        return
      }
      // TODO: Set semicolon loc.
      allMembers.append(member)

      // Hoist 'VarDecl' and 'EnumElementDecl' to the block.
      withBridgedSwiftClosure { ptr in
        let d = ptr!.load(as: BridgedDecl.self)
        allMembers.append(d)
      } call: { handle in
        member.forEachDeclToHoist(handle)
      }
    }

    return allMembers
  }

  /// Generate an inheritance clause, diagnosing the placement of a `class`
  /// requirement as `Parser::parseInheritance` does.
  func generate(
    inheritedTypeList node: InheritedTypeListSyntax,
    allowClassRequirement: Bool = false,
    allowAnyObject: Bool = false
  ) -> BridgedArrayRef {
    var firstClassRequirement: ClassRestrictionTypeSyntax? = nil
    // Entries the C++ parser drops after diagnosing them.
    var dropped = Set<SyntaxIdentifier>()
    for (index, entry) in node.enumerated() {
      guard let classRequirement = entry.type.as(ClassRestrictionTypeSyntax.self),
        classRequirement.classKeyword.presence == .present
      else {
        continue
      }
      if !allowClassRequirement {
        dropped.insert(entry.id)
        self.diagnose(.unexpected_class_constraint, at: classRequirement.classKeyword)
        if allowAnyObject {
          self.diagnose(
            .suggest_anyobject,
            at: classRequirement.classKeyword,
            fixIts: [.init(replace: self.generateCharSourceRange(classRequirement.classKeyword), with: "AnyObject")]
          )
        }
      } else if firstClassRequirement != nil {
        // If we already saw a class requirement, complain.
        dropped.insert(entry.id)
        self.diagnose(.redundant_class_requirement, at: classRequirement.classKeyword)
      } else {
        firstClassRequirement = classRequirement
        // If the class requirement was not the first requirement, complain.
        if index != 0 {
          self.diagnose(.late_class_requirement, at: classRequirement.classKeyword)
        }
      }
    }
    return node.compactMap(in: self) { entry in
      dropped.contains(entry.id) ? nil : self.generate(type: entry.type)
    }
  }

  @inline(__always)
  func generate(precedenceGroupNameList node: PrecedenceGroupNameListSyntax) -> BridgedArrayRef {
    node.lazy.map {
      self.generateLocatedIdentifier($0.name)
    }.bridgedArray(in: self)
  }
}

extension ExprSyntax {
  /// Whether this expression is one of the literal forms an enum case's raw
  /// value may take without the `LiteralExpressions` feature.
  ///
  /// Mirrors `isValidEnumRawValueLiteral` in ParseDecl.cpp. A negated numeric
  /// literal is written as a prefix operator applied to the literal, and the C++
  /// parser folds the sign into the literal while parsing, so accept that shape
  /// too.
  var isValidEnumRawValueLiteral: Bool {
    var expr = self
    if let prefix = expr.as(PrefixOperatorExprSyntax.self), prefix.operator.rawText == "-" {
      expr = prefix.expression
    }
    switch expr.as(ExprSyntaxEnum.self) {
    case .integerLiteralExpr, .floatLiteralExpr, .stringLiteralExpr,
      .booleanLiteralExpr, .nilLiteralExpr:
      return true
    default:
      return false
    }
  }
}

extension ASTGenVisitor {
  /// Diagnose misuse of `#sourceLocation`, as the C++ parser does.
  func check(poundSourceLocation node: PoundSourceLocationSyntax) {
    let wasInEnvironment = self.inPoundSourceLocationEnvironment
    self.inPoundSourceLocationEnvironment = false

    guard let arguments = node.arguments else {
      // `#sourceLocation()` closes the directive opened earlier.
      if !wasInEnvironment, let next = node.rightParen.nextToken(viewMode: .sourceAccurate) {
        self.diagnose(.unexpected_line_directive, at: next)
      }
      return
    }
    if let line = Int(arguments.lineNumber.text.filter { $0 != "_" }), line == 0 {
      self.diagnose(.line_directive_line_zero, at: arguments.lineNumber)
      return
    }
    self.inPoundSourceLocationEnvironment = true
  }
}

extension ASTGenVisitor {
  /// Whether \p node is a member of an enum, possibly inside `#if`.
  func isDirectlyInEnumBody(_ node: some SyntaxProtocol) -> Bool {
    var current = node.parent
    while let parent = current {
      if let block = parent.as(MemberBlockSyntax.self) {
        return block.parent?.is(EnumDeclSyntax.self) == true
      }
      current = parent.parent
    }
    return false
  }
}

extension ASTGenVisitor {
  /// Diagnose a declaration named with the reserved '$' prefix.
  func diagnoseDollarDeclName(_ token: TokenSyntax) {
    // Backticks do not exempt a name.
    let text = token.identifier?.name ?? token.text
    guard text.count > 1, text.hasPrefix("$"), token.presence == .present else {
      return
    }
    if self.ctx.allowsDollarIdentifierDecls(at: self.generateSourceLoc(token), in: self.declContext) {
      return
    }
    self.diagnose(.dollar_identifier_decl(self.generateIdentifier(token)), at: token)
  }
}
