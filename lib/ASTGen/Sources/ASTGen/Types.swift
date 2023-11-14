//===--- Types.swift ------------------------------------------------------===//
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

/// Check if an `TypeSyntax` can be generated using ASTGen.
///
/// If all the type nodes that shares the first token are migrated,
/// returns true.
func isTypeMigrated(_ node: TypeSyntax) -> Bool {
  var current: Syntax = Syntax(node)
  if let firstToken = node.firstToken(viewMode: .sourceAccurate) {
    current = firstToken.parent!
  }
  while true {
    switch current.kind {
    case // Known implemented kinds.
        .arrayType, .attributedType, .compositionType, .someOrAnyType,
        .dictionaryType, .functionType, .implicitlyUnwrappedOptionalType,
        .memberType, .metatypeType, .namedOpaqueReturnType, .optionalType,
        .packExpansionType, .identifierType, .tupleType:
      break
    case // Known unimplemented kinds.
        .suppressedType, .packElementType, .missingType:
      return false;
    case // Unknown type kinds
      _ where current.is(TypeSyntax.self):
      return false
    default:
      break
    }
    if current.id == node.id {
      return true
    }
    // This is walking up the parents from the first token of `node`. `.parent`
    // must exist if `current` is not `node`
    current = current.parent!
  }
}

extension ASTGenVisitor {
  func generate(type node: TypeSyntax) -> BridgedTypeRepr {
    guard isTypeMigrated(node) else {
      return self.generateWithLegacy(node)
    }
    switch node.as(TypeSyntaxEnum.self) {
    case .arrayType(let node):
      return self.generate(arrayType: node)
    case .attributedType(let node):
      return self.generate(attributedType: node)
    case .classRestrictionType:
      break
    case .compositionType(let node):
      return self.generate(compositionType: node)
    case .dictionaryType(let node):
      return self.generate(dictionaryType: node)
    case .functionType(let node):
      return self.generate(functionType: node)
    case .identifierType(let node):
      return self.generate(identifierType: node)
    case .implicitlyUnwrappedOptionalType(let node):
      return self.generate(implicitlyUnwrappedOptionalType: node)
    case .memberType(let node):
      return self.generate(memberType: node)
    case .metatypeType(let node):
      return self.generate(metatypeType: node)
    case .missingType:
      break
    case .namedOpaqueReturnType(let node):
      return self.generate(namedOpaqueReturnType: node)
    case .optionalType(let node):
      return self.generate(optionalType: node)
    case .packElementType:
      break
    case .packExpansionType(let node):
      return self.generate(packExpansionType: node)
    case .someOrAnyType(let node):
      return self.generate(someOrAnyType: node)
    case .suppressedType:
      break
    case .tupleType(let node):
      return self.generate(tupleType: node)
    }
    preconditionFailure("isTypeMigrated() mismatch")
  }

  public func generate(identifierType node: IdentifierTypeSyntax) -> BridgedTypeRepr {
    let loc = node.bridgedSourceLoc(in: self)

    // If this is the bare 'Any' keyword, produce an empty composition type.
    if node.name.tokenKind == .keyword(.Any) && node.genericArgumentClause == nil {
      return BridgedCompositionTypeRepr.createEmpty(self.ctx, anyKeywordLoc: loc)
    }

    let id = node.name.bridgedIdentifier(in: self)

    guard let generics = node.genericArgumentClause else {
      return BridgedSimpleIdentTypeRepr.createParsed(ctx, loc: loc, name: id)
    }

    let genericArguments = generics.arguments.lazy.map {
      self.generate(type: $0.argument)
    }

    return BridgedGenericIdentTypeRepr.createParsed(
      self.ctx,
      name: id,
      nameLoc: loc,
      genericArgs: genericArguments.bridgedArray(in: self),
      leftAngleLoc: generics.leftAngle.bridgedSourceLoc(in: self),
      rightAngleLoc: generics.rightAngle.bridgedSourceLoc(in: self)
    )
  }

  public func generate(memberType node: MemberTypeSyntax) -> BridgedTypeRepr {
    // Gather the member components, in decreasing depth order.
    var reverseMemberComponents = [BridgedTypeRepr]()

    var baseType = TypeSyntax(node)
    while let memberType = baseType.as(MemberTypeSyntax.self) {
      let (name, nameLoc) = memberType.name.bridgedIdentifierAndSourceLoc(in: self)

      if let generics = memberType.genericArgumentClause {
        let genericArguments = generics.arguments.lazy.map {
          self.generate(type: $0.argument)
        }

        reverseMemberComponents.append(
          BridgedGenericIdentTypeRepr.createParsed(
            self.ctx,
            name: name,
            nameLoc: nameLoc,
            genericArgs: genericArguments.bridgedArray(in: self),
            leftAngleLoc: generics.leftAngle.bridgedSourceLoc(in: self),
            rightAngleLoc: generics.rightAngle.bridgedSourceLoc(in: self)
          )
        )
      } else {
        reverseMemberComponents.append(
          BridgedSimpleIdentTypeRepr.createParsed(self.ctx, loc: nameLoc, name: name)
        )
      }

      baseType = memberType.baseType
    }

    let baseComponent = generate(type: baseType)
    let memberComponents = reverseMemberComponents.reversed().bridgedArray(in: self)

    return BridgedMemberTypeRepr.createParsed(
      self.ctx,
      base: baseComponent,
      members: memberComponents
    )
  }

  public func generate(arrayType node: ArrayTypeSyntax) -> BridgedTypeRepr {
    let elementType = generate(type: node.element)
    let lSquareLoc = node.leftSquare.bridgedSourceLoc(in: self)
    let rSquareLoc = node.rightSquare.bridgedSourceLoc(in: self)
    return BridgedArrayTypeRepr.createParsed(
      self.ctx,
      base: elementType,
      leftSquareLoc: lSquareLoc,
      rightSquareLoc: rSquareLoc
    )
  }

  public func generate(dictionaryType node: DictionaryTypeSyntax) -> BridgedTypeRepr {
    let keyType = generate(type: node.key)
    let valueType = generate(type: node.value)
    let colonLoc = node.colon.bridgedSourceLoc(in: self)
    let lSquareLoc = node.leftSquare.bridgedSourceLoc(in: self)
    let rSquareLoc = node.rightSquare.bridgedSourceLoc(in: self)
    return BridgedDictionaryTypeRepr.createParsed(
      self.ctx,
      leftSquareLoc: lSquareLoc,
      keyType: keyType,
      colonLoc: colonLoc,
      valueType: valueType,
      rightSquareLoc: rSquareLoc
    )
  }

  public func generate(metatypeType node: MetatypeTypeSyntax) -> BridgedTypeRepr {
    let baseType = generate(type: node.baseType)
    let tyLoc = node.metatypeSpecifier.bridgedSourceLoc(in: self)
    if node.metatypeSpecifier.text == "Type" {
      return BridgedMetatypeTypeRepr.createParsed(
        self.ctx,
        base: baseType,
        typeKeywordLoc: tyLoc
      )
    } else {
      assert(node.metatypeSpecifier.text == "Protocol")
      return BridgedProtocolTypeRepr.createParsed(
        self.ctx,
        base: baseType,
        protocolKeywordLoc: tyLoc
      )
    }
  }

  public func generate(implicitlyUnwrappedOptionalType node: ImplicitlyUnwrappedOptionalTypeSyntax) -> BridgedTypeRepr {
    let base = generate(type: node.wrappedType)
    let exclaimLoc = node.exclamationMark.bridgedSourceLoc(in: self)
    return BridgedImplicitlyUnwrappedOptionalTypeRepr.createParsed(
      self.ctx,
      base: base,
      exclaimLoc: exclaimLoc
    )
  }

  public func generate(optionalType node: OptionalTypeSyntax) -> BridgedTypeRepr {
    let base = generate(type: node.wrappedType)
    let questionLoc = node.questionMark.bridgedSourceLoc(in: self)
    return BridgedOptionalTypeRepr.createParsed(
      self.ctx,
      base: base,
      questionLoc: questionLoc
    )
  }

  public func generate(packExpansionType node: PackExpansionTypeSyntax) -> BridgedTypeRepr {
    let base = generate(type: node.repetitionPattern)
    let repeatLoc = node.repeatKeyword.bridgedSourceLoc(in: self)
    return BridgedPackExpansionTypeRepr.createParsed(
      self.ctx,
      base: base,
      repeatKeywordLoc: repeatLoc
    )
  }

  public func generate(tupleType node: TupleTypeSyntax) -> BridgedTypeRepr {
    BridgedTupleTypeRepr.createParsed(
      self.ctx,
      elements: self.generate(tupleTypeElementList: node.elements),
      leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
      rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
    )
  }

  public func generate(compositionType node: CompositionTypeSyntax) -> BridgedTypeRepr {
    assert(node.elements.count > 1)

    let types = node.elements.lazy.map {
      generate(type: $0.type)
    }

    return BridgedCompositionTypeRepr.createParsed(
      self.ctx,
      types: types.bridgedArray(in: self),
      ampersandLoc: (node.elements.first?.ampersand).bridgedSourceLoc(in: self)
    )
  }

  public func generate(functionType node: FunctionTypeSyntax) -> BridgedTypeRepr {
    BridgedFunctionTypeRepr.createParsed(
      self.ctx,
      // FIXME: Why does `FunctionTypeSyntax` not have a `TupleTypeSyntax` child?
      argsType: BridgedTupleTypeRepr.createParsed(
        self.ctx,
        elements: self.generate(tupleTypeElementList: node.parameters),
        leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
        rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
      ),
      asyncLoc: (node.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsLoc: (node.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(optional: node.effectSpecifiers?.thrownError?.type).asNullable,
      arrowLoc: node.returnClause.arrow.bridgedSourceLoc(in: self),
      resultType: generate(type: node.returnClause.type)
    )
  }

  public func generate(namedOpaqueReturnType node: NamedOpaqueReturnTypeSyntax) -> BridgedTypeRepr {
    let baseTy = generate(type: node.type)
    return BridgedNamedOpaqueReturnTypeRepr.createParsed(self.ctx, base: baseTy)
  }

  public func generate(someOrAnyType node: SomeOrAnyTypeSyntax) -> BridgedTypeRepr {
    let someOrAnyLoc = node.someOrAnySpecifier.bridgedSourceLoc(in: self)
    let baseTy = generate(type: node.constraint)
    if node.someOrAnySpecifier.text == "some" {
      return BridgedOpaqueReturnTypeRepr.createParsed(
        self.ctx,
        someKeywordLoc: someOrAnyLoc,
        base: baseTy
      )
    } else {
      assert(node.someOrAnySpecifier.text == "any")
      return BridgedExistentialTypeRepr.createParsed(
        self.ctx,
        anyKeywordLoc: someOrAnyLoc,
        base: baseTy
      )
    }
  }

  // NOTE: When implementing new `generate(type:)`, please update  `isTypeMigrated(_:)`.
}

// MARK: - SpecifierTypeRepr/AttributedTypeRepr

extension BridgedAttributedTypeSpecifier {
  fileprivate init?(from tokenKind: TokenKind) {
    switch tokenKind {
    case .keyword(.inout): self = .inOut
    case .keyword(.borrowing): self = .borrowing
    case .keyword(.consuming): self = .consuming
    case .keyword(.__shared): self = .legacyShared
    case .keyword(.__owned): self = .legacyOwned
    case .keyword(._const): self = .const
    case .keyword(.isolated): self = .isolated
    default: return nil
    }
  }
}

extension ASTGenVisitor {
  public func generate(attributedType node: AttributedTypeSyntax) -> BridgedTypeRepr {
    var type = generate(type: node.baseType)

    // Handle specifiers.
    if let specifier = node.specifier {
      if let kind = BridgedAttributedTypeSpecifier(from: specifier.tokenKind) {
        type = BridgedSpecifierTypeRepr.createParsed(
          self.ctx,
          base: type,
          specifier: kind,
          specifierLoc: specifier.bridgedSourceLoc(in: self)
        )
      } else {
        self.diagnose(Diagnostic(node: specifier, message: UnexpectedTokenKindError(token: specifier)))
      }
    }

    // Handle type attributes.
    if !node.attributes.isEmpty {
      let typeAttributes = BridgedTypeAttributes()
      for attributeElt in node.attributes {
        // FIXME: Ignoring #ifs entirely. We want to provide a filtered view,
        // but we don't have that ability right now.
        guard case let .attribute(attribute) = attributeElt else {
          continue
        }

        // Only handle simple attribute names right now.
        guard let identType = attribute.attributeName.as(IdentifierTypeSyntax.self) else {
          continue
        }

        let nameSyntax = identType.name
        var name = nameSyntax.text
        let typeAttrKind = name.withBridgedString { bridgedName in
          BridgedTypeAttrKind(from: bridgedName)
        }
        let atLoc = attribute.atSign.bridgedSourceLoc(in: self)
        let attrLoc = nameSyntax.bridgedSourceLoc(in: self)
        switch typeAttrKind {
        // SIL attributes
        // FIXME: Diagnose if not in SIL mode? Or should that move to the
        // type checker?
        case .out, .in, .owned, .unowned_inner_pointer, .guaranteed,
          .autoreleased, .callee_owned, .callee_guaranteed, .objc_metatype,
          .sil_weak, .sil_unowned, .inout, .block_storage, .box,
          .dynamic_self, .sil_unmanaged, .error, .error_indirect,
          .error_unowned, .direct, .inout_aliasable,
          .in_guaranteed, .in_constant, .captures_generics, .moveOnly:
          fallthrough

        case .autoclosure, .escaping, .noescape, .noDerivative, .async,
          .sendable, .retroactive, .unchecked, ._local, ._noMetadata,
          .pack_owned, .pack_guaranteed, .pack_inout, .pack_out,
          .pseudogeneric, .yields, .yield_once, .yield_many, .thin, .thick,
          .count, .unimplementable:
          typeAttributes.addSimpleAttr(kind: typeAttrKind, atLoc: atLoc, attrLoc: attrLoc)

        case .opened, .pack_element, .differentiable, .convention,
          ._opaqueReturnTypeOf:
          // FIXME: These require more complicated checks
          break
        }
      }

      type = BridgedAttributedTypeRepr.createParsed(
        self.ctx,
        base: type,
        consumingAttributes: typeAttributes
      )
    }

    return type
  }
}

extension ASTGenVisitor {
  func generate(tupleTypeElementList node: TupleTypeElementListSyntax) -> BridgedArrayRef {
    node.lazy.map { element in
      let (firstName, firstNameLoc) = element.firstName.bridgedIdentifierAndSourceLoc(in: self)
      let (secondName, secondNameLoc) = element.secondName.bridgedIdentifierAndSourceLoc(in: self)
      var type = generate(type: element.type)
      if let ellipsis = element.ellipsis {
        type = BridgedVarargTypeRepr.createParsed(
          self.ctx,
          base: type,
          ellipsisLoc: ellipsis.bridgedSourceLoc(in: self)
        )
      }

      return BridgedTupleTypeElement(
        Name: firstName,
        NameLoc: firstNameLoc,
        SecondName: secondName,
        SecondNameLoc: secondNameLoc,
        UnderscoreLoc: nil, /*N.B. Only important for SIL*/
        ColonLoc: element.colon.bridgedSourceLoc(in: self),
        Type: type,
        TrailingCommaLoc: element.trailingComma.bridgedSourceLoc(in: self)
      )
    }.bridgedArray(in: self)
  }
}

