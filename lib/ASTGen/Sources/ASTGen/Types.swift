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
      return self.generate(compositionType: node).asTypeRepr
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

  func generate(identifierType node: IdentifierTypeSyntax) -> BridgedTypeRepr {
    let loc = self.generateSourceLoc(node.name)

    // If this is the bare 'Any' keyword, produce an empty composition type.
    if node.name.keywordKind == .Any && node.genericArgumentClause == nil {
      return BridgedCompositionTypeRepr.createEmpty(self.ctx, anyKeywordLoc: loc).asTypeRepr
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

    let id = self.generateIdentifier(node.name)

    guard let generics = node.genericArgumentClause else {
      return BridgedUnqualifiedIdentTypeRepr.createParsed(ctx, loc: loc, name: id).asTypeRepr
    }

    let genericArguments = generics.arguments.lazy.map {
      self.generate(genericArgument: $0.argument)
    }

    return BridgedUnqualifiedIdentTypeRepr.createParsed(
      self.ctx,
      name: id,
      nameLoc: loc,
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
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.name)

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
      name: name,
      nameLoc: nameLoc,
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
      elements: self.generate(tupleTypeElementList: node.elements),
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
    .createParsed(
      self.ctx,
      // FIXME: Why does `FunctionTypeSyntax` not have a `TupleTypeSyntax` child?
      argsType: BridgedTupleTypeRepr.createParsed(
        self.ctx,
        elements: self.generate(tupleTypeElementList: node.parameters),
        leftParenLoc: self.generateSourceLoc(node.leftParen),
        rightParenLoc: self.generateSourceLoc(node.rightParen)
      ).asTypeRepr,
      asyncLoc: self.generateSourceLoc(node.effectSpecifiers?.asyncSpecifier),
      throwsLoc: self.generateSourceLoc(node.effectSpecifiers?.throwsClause?.throwsSpecifier),
      thrownType: self.generate(type: node.effectSpecifiers?.thrownError),
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
      loc: self.generateSourceLoc(node.classKeyword),
      name: self.ctx.getIdentifier("AnyObject")
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
      fatalError("expected identifier, 'self', or integer in @lifetime")
    }
  }

  func generate(attributedType node: AttributedTypeSyntax) -> BridgedTypeRepr {
    var type = generate(type: node.baseType)

    // Specifiers
    var ownership: BridgedParamSpecifier = .default
    var ownershipLoc: SourceLoc = nil
    var isolatedLoc: SourceLoc = nil
    var constLoc: SourceLoc = nil
    var sendingLoc: SourceLoc = nil
    var lifetimeEntry: BridgedLifetimeEntry? = nil
    var nonisolatedLoc: SourceLoc = nil

    // TODO: Diagnostics for duplicated specifiers, and ordering.
    for node in node.specifiers {
      let loc = self.generateSourceLoc(node)
      switch node {
      case .simpleTypeSpecifier(let node):
        switch node.specifier.keywordKind {
        case .inout:
          (ownership, ownershipLoc) = (.inOut, loc)
        case .__shared:
          (ownership, ownershipLoc) = (.legacyShared, loc)
        case .__owned:
          (ownership, ownershipLoc) = (.legacyOwned, loc)
        case .borrowing:
          (ownership, ownershipLoc) = (.borrowing, loc)
        case .consuming:
          (ownership, ownershipLoc) = (.consuming, loc)
        case .isolated:
          isolatedLoc = loc
        case ._const:
          constLoc = loc
        case .sending:
          sendingLoc = loc
        default:
          // TODO: Diagnostics.
          fatalError("(compiler bug) unrecognized type specifier")
        }
      case .lifetimeTypeSpecifier(let node):
        lifetimeEntry = .createParsed(
          self.ctx,
          range: self.generateSourceRange(
            start: node.dependsOnKeyword,
            end: node.rightParen
          ),
          sources: node.arguments.lazy.compactMap(self.generateLifetimeDescriptor(lifetimeSpecifierArgument:)).bridgedArray(in: self)
        )
      case .nonisolatedTypeSpecifier(_):
        nonisolatedLoc = loc
      }
    }

    // Attributes.
    let typeAttributes = self.generateTypeAttributes(node)

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
      type = BridgedCallerIsolatedTypeRepr.createParsed(
        self.ctx,
        base: type,
        specifierLoc: nonisolatedLoc
      ).asTypeRepr
    }

    return type
  }
}

extension ASTGenVisitor {
  func generate(tupleTypeElementList node: TupleTypeElementListSyntax) -> BridgedArrayRef {
    node.lazy.map { element in
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
    }.bridgedArray(in: self)
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
      let name = self.generateIdentifierAndSourceLoc(node.baseName)
      return BridgedUnqualifiedIdentTypeRepr .createParsed(
        self.ctx,
        name: name.identifier,
        nameLoc: name.sourceLoc,
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
      let name = self.generateIdentifierAndSourceLoc(node.declName.baseName)
      return BridgedDeclRefTypeRepr.createParsed(
        self.ctx,
        base: base,
        name: name.identifier,
        nameLoc: name.sourceLoc,
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

    case .sequenceExpr(let node):
      // TODO: Support composition type?
      _ = node
      break

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

    // TODO: Diagnose
    fatalError("invalid/unimplemented expression for type")
  }
}
