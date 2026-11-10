//===--- Types.swift ------------------------------------------------------===//
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
@_spi(ExperimentalLanguageFeatures) @_spi(RawSyntax) import SwiftSyntax

extension EffectSpecifiersSyntax {
  var thrownError: TypeSyntax? {
    throwsClause?.type
  }
}

extension ASTGenVisitor {
  func generate(type node: TypeSyntax) -> BridgedTypeRepr {
    switch node.as(TypeSyntaxEnum.self) {
    case .arrayType(let node):
      return self.generate(arrayType: node).asTypeRepr
    case .attributedType(let node):
      return self.generate(attributedType: node)
    case .classRestrictionType(let node):
      return self.generate(classRestrictionType: node).asTypeRepr
    case .compositionType(let node):
      let composition = self.generate(compositionType: node).asTypeRepr
      // `P & some Q` is diagnosed by the parser, which then makes the whole
      // composition opaque (or existential), as if the keyword were in front.
      if node.parent?.is(SomeOrAnyTypeSyntax.self) != true,
        let misplaced = node.elements.lazy.compactMap({
          $0.unexpectedBeforeType?.compactMap { $0.as(TokenSyntax.self) }.first
        }).first
      {
        let loc = self.generateSourceLoc(misplaced)
        if misplaced.text == "some" {
          return BridgedOpaqueReturnTypeRepr.createParsed(self.ctx, someKeywordLoc: loc, base: composition).asTypeRepr
        }
        return BridgedExistentialTypeRepr.createParsed(self.ctx, anyKeywordLoc: loc, base: composition).asTypeRepr
      }
      return composition
    case .dictionaryType(let node):
      return self.generate(dictionaryType: node).asTypeRepr
    case .functionType(let node):
      return self.generate(functionType: node).asTypeRepr
    case .identifierType(let node):
      return self.generate(identifierType: node)
    case .inlineArrayType(let node):
      return self.generate(inlineArrayType: node).asTypeRepr
    case .implicitlyUnwrappedOptionalType(let node):
      return self.generate(implicitlyUnwrappedOptionalType: node).asTypeRepr
    case .memberType(let node):
      // `Int.` without a member name has been diagnosed by the parser; there is
      // nothing to look up.
      if node.name.presence == .missing {
        return BridgedErrorTypeRepr.create(self.ctx, range: self.generateSourceRange(node)).asTypeRepr
      }
      return self.generate(memberType: node).asTypeRepr
    case .metatypeType(let node):
      return self.generate(metatypeType: node)
    case .missingType(let node):
      return self.generate(missingType: node)
    case .namedOpaqueReturnType(let node):
      return self.generate(namedOpaqueReturnType: node).asTypeRepr
    case .optionalType(let node):
      return self.generate(optionalType: node).asTypeRepr
    case .packElementType(let node):
      return self.generate(packElementType: node).asTypeRepr
    case .packExpansionType(let node):
      return self.generate(packExpansionType: node).asTypeRepr
    case .someOrAnyType(let node):
      return self.generate(someOrAnyType: node)
    case .suppressedType(let node):
      return self.generate(suppressedType: node).asTypeRepr
    case .tupleType(let node):
      return self.generate(tupleType: node).asTypeRepr
    }
  }

  /// `protocol<A, B>`, which was removed in favor of `A & B`. The parser reads
  /// it as a generic type with an unexpected `protocol` keyword and diagnoses
  /// it; build the composition it stands for.
  func generate(legacyProtocolComposition node: IdentifierTypeSyntax, generics: GenericArgumentClauseSyntax) -> BridgedTypeRepr {
    let protocolLoc = self.generateSourceLoc(node.unexpectedBetweenModuleSelectorAndName)
    let components = generics.arguments.compactMap { argument -> TypeSyntax? in
      if case .type(let type) = argument.argument, !type.is(MissingTypeSyntax.self) { return type }
      return nil
    }

    if components.isEmpty {
      return BridgedCompositionTypeRepr.createEmpty(self.ctx, anyKeywordLoc: protocolLoc).asTypeRepr
    }
    if components.count == 1 {
      return self.generate(type: components[0])
    }
    return BridgedCompositionTypeRepr.createParsed(
      self.ctx,
      types: components.lazy.map { self.generate(type: $0) }.bridgedArray(in: self),
      // The composition starts at `protocol`.
      ampersandLoc: protocolLoc
    ).asTypeRepr
  }

  func generate(identifierType node: IdentifierTypeSyntax) -> BridgedTypeRepr {
    let loc = self.generateSourceLoc(node.name)

    // If this is the bare 'Any' keyword, produce an empty composition type.
    if node.name.keywordKind == .Any && node.genericArgumentClause == nil {
      return BridgedCompositionTypeRepr.createEmpty(self.ctx, anyKeywordLoc: loc).asTypeRepr
    }
    if let unexpected = node.unexpectedBetweenModuleSelectorAndName,
      unexpected.count == 1, unexpected.first?.as(TokenSyntax.self)?.keywordKind == .protocol,
      node.name.presence == .missing,
      let generics = node.genericArgumentClause
    {
      return self.generate(legacyProtocolComposition: node, generics: generics)
    }
    if node.name.rawText == "_" {
      guard node.genericArgumentClause == nil else {
        // TODO: Diagnose.
        fatalError()
        // return BridgedErrorTypeRepr.create()
      }
      return BridgedPlaceholderTypeRepr.createParsed(
        self.ctx,
        loc: loc
      ).asTypeRepr
    }

    let nameRef = self.generateDeclNameRef(moduleSelector: node.moduleSelector, baseName: node.name)

    guard let generics = node.genericArgumentClause else {
      return BridgedUnqualifiedIdentTypeRepr.createParsed(ctx, name: nameRef.name, loc: nameRef.loc).asTypeRepr
    }

    let genericArguments = generics.arguments.lazy.map {
      self.generate(genericArgument: $0.argument)
    }

    return BridgedUnqualifiedIdentTypeRepr.createParsed(
      self.ctx,
      name: nameRef.name,
      nameLoc: nameRef.loc,
      genericArgs: genericArguments.bridgedArray(in: self),
      leftAngleLoc: self.generateSourceLoc(generics.leftAngle),
      rightAngleLoc: self.generateSourceLoc(generics.rightAngle)
    ).asTypeRepr
  }

  func generate(inlineArrayType node: InlineArrayTypeSyntax) -> BridgedInlineArrayTypeRepr {
    .createParsed(
      self.ctx,
      count: self.generate(genericArgument: node.count.argument),
      element: self.generate(genericArgument: node.element.argument),
      brackets: .init(
        start: self.generateSourceLoc(node.leftSquare),
        end: self.generateSourceLoc(node.rightSquare)
      )
    )
  }

  func generate(memberType node: MemberTypeSyntax) -> BridgedDeclRefTypeRepr {
    let nameRef = self.generateDeclNameRef(moduleSelector: node.moduleSelector, baseName: node.name)

    let genericArguments: BridgedArrayRef
    let angleRange: SourceRange
    if let generics = node.genericArgumentClause {
      genericArguments = generics.arguments.lazy.map {
        self.generate(genericArgument: $0.argument)
      }.bridgedArray(in: self)

      angleRange = self.generateSourceRange(start: generics.leftAngle, end: generics.rightAngle)
    } else {
      genericArguments = .init()
      angleRange = .init()
    }

    return BridgedDeclRefTypeRepr.createParsed(
      self.ctx,
      base: self.generate(type: node.baseType),
      name: nameRef.name,
      nameLoc: nameRef.loc,
      genericArguments: genericArguments,
      angleRange: angleRange
    )
  }

  func generate(arrayType node: ArrayTypeSyntax) -> BridgedArrayTypeRepr {
    let elementType = generate(type: node.element)
    let lSquareLoc = self.generateSourceLoc(node.leftSquare)
    let rSquareLoc = self.generateSourceLoc(node.rightSquare)
    return .createParsed(
      self.ctx,
      base: elementType,
      leftSquareLoc: lSquareLoc,
      rightSquareLoc: rSquareLoc
    )
  }

  func generate(dictionaryType node: DictionaryTypeSyntax) -> BridgedDictionaryTypeRepr {
    let keyType = self.generate(type: node.key)
    let valueType = self.generate(type: node.value)
    let colonLoc = self.generateSourceLoc(node.colon)
    let lSquareLoc = self.generateSourceLoc(node.leftSquare)
    let rSquareLoc = self.generateSourceLoc(node.rightSquare)
    return .createParsed(
      self.ctx,
      leftSquareLoc: lSquareLoc,
      keyType: keyType,
      colonLoc: colonLoc,
      valueType: valueType,
      rightSquareLoc: rSquareLoc
    )
  }

  func generate(metatypeType node: MetatypeTypeSyntax) -> BridgedTypeRepr {
    let baseType = generate(type: node.baseType)
    let tyLoc = self.generateSourceLoc(node.metatypeSpecifier)
    if node.metatypeSpecifier.keywordKind == .Type {
      return BridgedMetatypeTypeRepr.createParsed(
        self.ctx,
        base: baseType,
        typeKeywordLoc: tyLoc
      ).asTypeRepr
    } else {
      assert(node.metatypeSpecifier.keywordKind == .Protocol)
      return BridgedProtocolTypeRepr.createParsed(
        self.ctx,
        base: baseType,
        protocolKeywordLoc: tyLoc
      ).asTypeRepr
    }
  }

  func generate(missingType node: MissingTypeSyntax) -> BridgedTypeRepr {
    let loc = self.generateSourceLoc(node.previousToken(viewMode: .sourceAccurate))
    return BridgedErrorTypeRepr.create(
      self.ctx,
      range: .init(start: loc)
    ).asTypeRepr
  }

  func generate(
    implicitlyUnwrappedOptionalType node: ImplicitlyUnwrappedOptionalTypeSyntax
  ) -> BridgedImplicitlyUnwrappedOptionalTypeRepr {
    let base = generate(type: node.wrappedType)
    let exclaimLoc = self.generateSourceLoc(node.exclamationMark)
    return .createParsed(
      self.ctx,
      base: base,
      exclaimLoc: exclaimLoc
    )
  }

  func generate(optionalType node: OptionalTypeSyntax) -> BridgedOptionalTypeRepr {
    let base = generate(type: node.wrappedType)
    let questionLoc = self.generateSourceLoc(node.questionMark)
    return .createParsed(
      self.ctx,
      base: base,
      questionLoc: questionLoc
    )
  }

  func generate(packElementType node: PackElementTypeSyntax) -> BridgedPackElementTypeRepr {
    let base = generate(type: node.pack)
    let eachLoc = self.generateSourceLoc(node.eachKeyword)
    return .createParsed(
      self.ctx,
      base: base,
      eachKeywordLoc: eachLoc
    )
  }

  func generate(packExpansionType node: PackExpansionTypeSyntax) -> BridgedPackExpansionTypeRepr {
    let base = generate(type: node.repetitionPattern)
    let repeatLoc = self.generateSourceLoc(node.repeatKeyword)
    return .createParsed(
      self.ctx,
      base: base,
      repeatKeywordLoc: repeatLoc
    )
  }

  func generate(tupleType node: TupleTypeSyntax) -> BridgedTupleTypeRepr {
    .createParsed(
      self.ctx,
      elements: self.generate(tupleTypeElementList: node.elements, isFunctionType: false),
      leftParenLoc: self.generateSourceLoc(node.leftParen),
      rightParenLoc: self.generateSourceLoc(node.rightParen)
    )
  }

  func generate(compositionType node: CompositionTypeSyntax) -> BridgedCompositionTypeRepr {
    assert(node.elements.count > 1)

    let types = node.elements.lazy.map {
      generate(type: $0.type)
    }

    return .createParsed(
      self.ctx,
      types: types.bridgedArray(in: self),
      ampersandLoc: self.generateSourceLoc(node.elements.first?.ampersand)
    )
  }

  func generate(functionType node: FunctionTypeSyntax) -> BridgedFunctionTypeRepr {
    var yieldsType : BridgedTypeRepr? = nil
    if let yields = node.yieldClause?.yields {
      yieldsType = BridgedTupleTypeRepr.createParsed(
        self.ctx,
        elements: yields.lazy.map { element in
          let type = generate(type: element.type)
          return BridgedTupleTypeElement(
            Name: nil,
            NameLoc: nil,
            SecondName: nil,
            SecondNameLoc: nil,
            UnderscoreLoc: nil,
            ColonLoc: nil,
            Type: type,
            TrailingCommaLoc: self.generateSourceLoc(element.trailingComma)
          )
        }.bridgedArray(in: self),
        leftParenLoc: self.generateSourceLoc(node.yieldClause?.leftParen),
        rightParenLoc: self.generateSourceLoc(node.yieldClause?.rightParen)
      ).asTypeRepr
    }  
    return BridgedFunctionTypeRepr.createParsed(
      self.ctx,
      // FIXME: Why does `FunctionTypeSyntax` not have a `TupleTypeSyntax` child?
      argsType: BridgedTupleTypeRepr.createParsed(
        self.ctx,
        elements: self.generate(tupleTypeElementList: node.parameters, isFunctionType: true),
        leftParenLoc: self.generateSourceLoc(node.leftParen),
        rightParenLoc: self.generateSourceLoc(node.rightParen)
      ).asTypeRepr,
      asyncLoc: self.generateSourceLoc(node.effectSpecifiers?.asyncSpecifier),
      throwsLoc: self.generateSourceLoc(node.effectSpecifiers?.throwsClause?.throwsSpecifier),
      thrownType: self.generate(type: node.effectSpecifiers?.thrownError),
      yieldsType: yieldsType.asNullable,
      arrowLoc: self.generateSourceLoc(node.returnClause.arrow),
      resultType: generate(type: node.returnClause.type)
    )
  }

  func generate(namedOpaqueReturnType node: NamedOpaqueReturnTypeSyntax) -> BridgedNamedOpaqueReturnTypeRepr {
    let genericParams = self.generate(genericParameterClause: node.genericParameterClause)
    let baseTy = generate(type: node.type)
    return .createParsed(self.ctx, base: baseTy, genericParamList: genericParams)
  }

  func generate(someOrAnyType node: SomeOrAnyTypeSyntax) -> BridgedTypeRepr {
    let someOrAnyLoc = self.generateSourceLoc(node.someOrAnySpecifier)
    let baseTy = generate(type: node.constraint)
    if node.someOrAnySpecifier.keywordKind == .some {
      return BridgedOpaqueReturnTypeRepr.createParsed(
        self.ctx,
        someKeywordLoc: someOrAnyLoc,
        base: baseTy
      ).asTypeRepr
    } else {
      assert(node.someOrAnySpecifier.keywordKind == .any)
      return BridgedExistentialTypeRepr.createParsed(
        self.ctx,
        anyKeywordLoc: someOrAnyLoc,
        base: baseTy
      ).asTypeRepr
    }
  }

  func generate(suppressedType node: SuppressedTypeSyntax) -> BridgedInverseTypeRepr {
    return .createParsed(
      self.ctx,
      tildeLoc: self.generateSourceLoc(node.withoutTilde),
      constraint: self.generate(type: node.type)
    )
  }

  func generate(classRestrictionType node: ClassRestrictionTypeSyntax) -> BridgedUnqualifiedIdentTypeRepr {
    // TODO: diagnostics.
    // warning: using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead
    return .createParsed(
      self.ctx,
      name: BridgedDeclNameRef.createParsed(
        self.ctx,
        moduleSelector: nil,
        baseName: .init(self.ctx.getIdentifier("AnyObject"))
      ),
      loc: BridgedDeclNameLoc.createParsed(self.generateSourceLoc(node.classKeyword))
    )
  }

  // NOTE: When implementing new `generate(type:)`, please update  `isTypeMigrated(_:)`.
}

// MARK: - SpecifierTypeRepr/AttributedTypeRepr

extension ASTGenVisitor {
  func generateLifetimeDescriptor(lifetimeSpecifierArgument node: LifetimeSpecifierArgumentSyntax) -> BridgedLifetimeDescriptor? {
    switch node.parameter.rawTokenKind {
    case .identifier, .keyword:
      return self.generateLifetimeDescriptor(
        nameToken: node.parameter,
        lifetimeDependenceKind: .default
      )
    case .integerLiteral:
      guard let index = Int(node.parameter.text) else {
        // TODO: Diagnose.
        fatalError("(compiler bug) invalid integer literal")
      }
      return.forOrdered(
        index,
        dependenceKind: .default,
        loc: self.generateSourceLoc(node.parameter)
      )
    default:
      // TODO: Diagnose.
      fatalError("expected identifier, 'self', or integer in @_lifetime")
    }
  }

  func generate(attributedType node: AttributedTypeSyntax) -> BridgedTypeRepr {
    var type = generate(type: node.baseType)

    // Specifiers
    var ownership: BridgedParamSpecifier = .default
    var ownershipLoc: SourceLoc = nil
    var ownershipSpecifier: TokenSyntax? = nil
    var isolatedLoc: SourceLoc = nil
    var constLoc: SourceLoc = nil
    var sendingLoc: SourceLoc = nil
    var lifetimeEntry: BridgedLifetimeEntry? = nil
    var nonisolatedLoc: SourceLoc = nil
    var specifierAttributes: [BridgedTypeOrCustomAttr] = []

    for node in node.specifiers {
      let loc = self.generateSourceLoc(node)
      switch node {
      case .simpleTypeSpecifier(let node):
        let token = node.specifier
        let newOwnership: BridgedParamSpecifier
        switch token.keywordKind {
        case .inout: newOwnership = .inOut
        case .__shared: newOwnership = .legacyShared
        case .__owned: newOwnership = .legacyOwned
        case .borrowing: newOwnership = .borrowing
        case .consuming: newOwnership = .consuming
        case .isolated:
          if isolatedLoc.isValid {
            self.diagnose(
              .parameter_specifier_repeated,
              at: token,
              fixIts: [self.tidyRemovalFixIt(token)]
            )
          }
          isolatedLoc = loc
          continue
        case ._const:
          constLoc = loc
          continue
        case .sending:
          // Only allow 'sending' to be written once.
          if sendingLoc.isValid {
            self.diagnose(
              .sending_repeated,
              at: token,
              fixIts: [.init(remove: self.generateCharSourceRange(token))]
            )
          }
          // 'sending' cannot be combined with 'borrowing'.
          if ownershipLoc.isValid && ownership == .borrowing {
            self.diagnose(.sending_cannot_be_used_with_borrowing("sending"), at: token)
          }
          sendingLoc = loc
          continue
        default:
          // TODO: Diagnostics.
          fatalError("(compiler bug) unrecognized type specifier")
        }

        if ownershipLoc.isValid {
          // Only one of 'inout', 'borrowing' and 'consuming' is allowed. The
          // first one wins, as in the C++ parser.
          self.diagnose(
            .parameter_specifier_repeated,
            at: token,
            // A specifier synthesized by recovery has no text to remove.
            fixIts: ownershipSpecifier!.presence == .present ? [self.tidyRemovalFixIt(ownershipSpecifier!)] : []
          )
        } else {
          (ownership, ownershipLoc, ownershipSpecifier) = (newOwnership, loc, token)
          // A specifier must come before 'sending'.
          if sendingLoc.isValid {
            self.diagnose(
              .sending_before_parameter_specifier(String(syntaxText: token.rawText)),
              at: token
            )
          }
        }
      case .lifetimeTypeSpecifier(let node):
        lifetimeEntry = .createParsed(
          self.ctx,
          range: self.generateSourceRange(
            start: node.dependsOnKeyword,
            end: node.rightParen
          ),
          // `compactMap(in:)`, not `lazy.compactMap { }.bridgedArray`, which
          // would run the transform -- and any diagnostic it emits -- twice.
          sources: node.arguments.compactMap(in: self) {
            self.generateLifetimeDescriptor(lifetimeSpecifierArgument: $0)
          }
        )
      case .nonisolatedTypeSpecifier(let specifier):
        if specifier.argument != nil {
          // `nonisolated(nonsending)`.
          nonisolatedLoc = loc
        } else {
          // A bare `nonisolated`, as in `extension C: nonisolated P`, is a
          // simple type attribute.
          specifierAttributes.append(
            .typeAttr(
              BridgedTypeAttribute.createSimple(
                self.ctx, kind: .Nonisolated, atLoc: nil,
                nameLoc: self.generateSourceLoc(specifier.nonisolatedKeyword))))
        }
      }
    }

    // Attributes.
    let typeAttributes = specifierAttributes + self.generateTypeAttributes(node)

    if !typeAttributes.isEmpty {
      type =
        BridgedAttributedTypeRepr.createParsed(
          self.ctx,
          base: type,
          attributes: typeAttributes.lazy.bridgedArray(in: self)
        ).asTypeRepr
    }

    if ownershipLoc.isValid && ownership != .default {
      type = BridgedOwnershipTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifier: ownership,
        specifierLoc: ownershipLoc
      ).asTypeRepr
    }

    if isolatedLoc.isValid {
      type = BridgedIsolatedTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifierLoc: isolatedLoc
      ).asTypeRepr
    }

    if constLoc.isValid {
      type = BridgedCompileTimeLiteralTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifierLoc: constLoc
      ).asTypeRepr
    }

    if sendingLoc.isValid {
      type = BridgedSendingTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifierLoc: sendingLoc
      ).asTypeRepr
    }

    if let lifetimeEntry {
      type = BridgedLifetimeDependentTypeRepr.createParsed(
        self.ctx,
        base: type,
        entry: lifetimeEntry
      ).asTypeRepr
    }

    if nonisolatedLoc.isValid {
      type = BridgedNonisolatedNonsendingTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifierLoc: nonisolatedLoc
      ).asTypeRepr
    }

    return type
  }
}

extension ASTGenVisitor {
  /// Diagnose the labels of a tuple type element, as `Parser::parseTupleTypeBody`
  /// does: a function type's parameters have no argument labels, and a tuple
  /// element may have only one label.
  private func diagnoseTupleTypeElementLabels(_ element: TupleTypeElementSyntax, isFunctionType: Bool) {
    guard let firstName = element.firstName else { return }
    let hasFirstLabel = firstName.rawTokenKind != .wildcard
    if !isFunctionType {
      if element.secondName != nil {
        // `(a b: Int)`: remove the second label.
        self.diagnose(
          .tuple_type_multiple_labels,
          at: firstName,
          fixIts: [.init(remove: self.labelRemovalRange(element, firstName: firstName, hasFirstLabel: hasFirstLabel))]
        )
      }
      return
    }

    // Arguments in function types are always unlabeled.
    if hasFirstLabel {
      let fixIt: CompilerDiagnosticFixIt
      if element.secondName == nil {
        fixIt = .init(
          replace: CharSourceRange(start: self.generateSourceLoc(firstName), byteLength: 0), with: "_ ")
      } else if element.secondName?.rawTokenKind == .wildcard {
        fixIt = .init(remove: self.labelRemovalRange(element, firstName: firstName, hasFirstLabel: false))
      } else {
        fixIt = .init(replace: self.generateCharSourceRange(firstName), with: "_")
      }
      self.diagnose(
        .function_type_argument_label(self.generateIdentifier(firstName)),
        at: firstName,
        fixIts: [fixIt]
      )
    }
  }

  /// The range to delete to drop a tuple element's extra label.
  private func labelRemovalRange(
    _ element: TupleTypeElementSyntax, firstName: TokenSyntax, hasFirstLabel: Bool
  ) -> CharSourceRange {
    if !hasFirstLabel {
      // `_ b: Int`: remove from the first name up to the type.
      return self.generateCharSourceRange(
        start: firstName.positionAfterSkippingLeadingTrivia,
        length: SourceLength(
          utf8Length: element.type.positionAfterSkippingLeadingTrivia.utf8Offset
            - firstName.positionAfterSkippingLeadingTrivia.utf8Offset))
    }
    // `a b: Int`: remove the text between the first name and the second.
    let start = firstName.endPositionBeforeTrailingTrivia
    let end = element.secondName!.endPositionBeforeTrailingTrivia
    return self.generateCharSourceRange(
      start: start, length: SourceLength(utf8Length: end.utf8Offset - start.utf8Offset))
  }

  func generate(tupleTypeElementList node: TupleTypeElementListSyntax, isFunctionType: Bool) -> BridgedArrayRef {
    node.compactMap(in: self) { element -> BridgedTupleTypeElement? in
      self.diagnoseTupleTypeElementLabels(element, isFunctionType: isFunctionType)
      let (firstName, firstNameLoc) =
        self.generateIdentifierAndSourceLoc(element.firstName)
      let (secondName, secondNameLoc) = self.generateIdentifierAndSourceLoc(element.secondName)
      var type = generate(type: element.type)
      if let ellipsis = element.ellipsis {
        type =
          BridgedVarargTypeRepr.createParsed(
            self.ctx,
            base: type,
            ellipsisLoc: self.generateSourceLoc(ellipsis)
          ).asTypeRepr
      }

      return BridgedTupleTypeElement(
        Name: firstName,
        NameLoc: firstNameLoc,
        SecondName: secondName,
        SecondNameLoc: secondNameLoc,
        UnderscoreLoc: nil, /*N.B. Only important for SIL*/
        ColonLoc: self.generateSourceLoc(element.colon),
        Type: type,
        TrailingCommaLoc: self.generateSourceLoc(element.trailingComma)
      )
    }
  }
}

extension ASTGenVisitor {
  struct GeneratedGenericArguments {
    var arguments: BridgedArrayRef = .init()
    var range: SourceRange = .init()
  }

  /// Generate 'TypeRepr' from a expression, because 'conformances' arguments in
  /// macro role attributes are parsed as normal expressions.
  func generateTypeRepr(
    expr node: ExprSyntax,
    genericArgs: GeneratedGenericArguments = GeneratedGenericArguments()
  ) -> BridgedTypeRepr? {
    if !genericArgs.arguments.isEmpty {
      guard node.is(MemberAccessExprSyntax.self) || node.is(DeclReferenceExprSyntax.self) else {
        // TODO: Diagnose.
        fatalError("generic arguments cannot be applied")
      }
    }

    switch node.as(ExprSyntaxEnum.self) {

    case .typeExpr(let node):
      return self.generate(type: node.type)

    case .declReferenceExpr(let node):
      guard node.argumentNames == nil else {
        // 'Foo.bar(_:baz:)'
        break
      }
      let nameRef = self.generateDeclNameRef(declReferenceExpr: node)
      return BridgedUnqualifiedIdentTypeRepr .createParsed(
        self.ctx,
        name: nameRef.name,
        nameLoc: nameRef.loc,
        genericArgs: genericArgs.arguments,
        leftAngleLoc: genericArgs.range.start,
        rightAngleLoc: genericArgs.range.end
      ).asTypeRepr

    case .memberAccessExpr(let node):
      guard let parsedBase = node.base else {
        // Implicit member expressions. E.g. '.Foo'
        break
      }
      guard let base = self.generateTypeRepr(expr: parsedBase) else {
        // Unsupported base expr. E.g. 'foo().bar'
        return nil
      }
      guard node.declName.argumentNames == nil else {
        // Function name. E.g. 'Foo.bar(_:baz:)'
        break
      }
      let nameRef = self.generateDeclNameRef(declReferenceExpr: node.declName)
      return BridgedDeclRefTypeRepr.createParsed(
        self.ctx,
        base: base,
        name: nameRef.name,
        nameLoc: nameRef.loc,
        genericArguments: genericArgs.arguments,
        angleRange: genericArgs.range
      ).asTypeRepr

    case .genericSpecializationExpr(let node):
      let args = node.genericArgumentClause.arguments.lazy.map {
        self.generate(genericArgument: $0.argument)
      }
      return self.generateTypeRepr(
        expr: node.expression,
        genericArgs: GeneratedGenericArguments(
          arguments: args.bridgedArray(in: self),
          range: self.generateSourceRange(node.genericArgumentClause)
        )
      )

    case .prefixOperatorExpr(let node) where node.operator.rawText == "~":
      // '~Copyable' in a position where the parser was expecting an expression,
      // e.g. the 'except:' argument of '@_preInverseGenerics'.
      guard let constraint = self.generateTypeRepr(expr: node.expression) else {
        break
      }
      return BridgedInverseTypeRepr.createParsed(
        self.ctx,
        tildeLoc: self.generateSourceLoc(node.operator),
        constraint: constraint
      ).asTypeRepr

    case .sequenceExpr(let node):
      // A composition written where the parser was expecting an expression, e.g.
      // '~Copyable & ~Escapable' as the 'except:' argument of
      // '@_preInverseGenerics'. The elements alternate between operands and '&'.
      var types: [BridgedTypeRepr] = []
      var firstAmpersandLoc: SourceLoc = nil
      for (index, element) in node.elements.enumerated() {
        if index % 2 == 1 {
          guard let binaryOperator = element.as(BinaryOperatorExprSyntax.self),
            binaryOperator.operator.rawText == "&"
          else {
            // Some other operator, so this is not a composition.
            return nil
          }
          if firstAmpersandLoc == nil {
            firstAmpersandLoc = self.generateSourceLoc(binaryOperator.operator)
          }
          continue
        }
        guard let type = self.generateTypeRepr(expr: element) else {
          return nil
        }
        types.append(type)
      }
      // An even number of elements means a trailing '&' with nothing after it;
      // the parser has already diagnosed that.
      guard types.count > 1, node.elements.count == types.count * 2 - 1 else {
        return nil
      }
      return BridgedCompositionTypeRepr.createParsed(
        self.ctx,
        types: types.lazy.bridgedArray(in: self),
        ampersandLoc: firstAmpersandLoc
      ).asTypeRepr

    case .tupleExpr(let node):
      // TODO: Support tuple type?
      _ = node
      break

    case .arrowExpr(let node):
      // TODO: Support function type?
      _ = node
      break

    default:
      break
    }

    // Not a type. Every caller checks for nil and reports something appropriate
    // for the position, so there is nothing to diagnose here.
    return nil
  }
}
