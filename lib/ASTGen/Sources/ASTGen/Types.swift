import ASTBridging
import BasicBridging
import SwiftDiagnostics
@_spi(ExperimentalLanguageFeatures) import SwiftSyntax

extension ASTGenVisitor {
  public func generate(_ node: IdentifierTypeSyntax) -> BridgedTypeRepr {
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
      self.generate($0.argument)
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

  public func generate(_ node: MemberTypeSyntax) -> BridgedTypeRepr {
    // Gather the member components, in decreasing depth order.
    var reverseMemberComponents = [BridgedTypeRepr]()

    var baseType = TypeSyntax(node)
    while let memberType = baseType.as(MemberTypeSyntax.self) {
      let (name, nameLoc) = memberType.name.bridgedIdentifierAndSourceLoc(in: self)

      if let generics = memberType.genericArgumentClause {
        let genericArguments = generics.arguments.lazy.map {
          self.generate($0.argument)
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

    let baseComponent = generate(baseType)
    let memberComponents = reverseMemberComponents.reversed().bridgedArray(in: self)

    return BridgedMemberTypeRepr.createParsed(
      self.ctx,
      base: baseComponent,
      members: memberComponents
    )
  }

  public func generate(_ node: ArrayTypeSyntax) -> BridgedTypeRepr {
    let elementType = generate(node.element)
    let lSquareLoc = node.leftSquare.bridgedSourceLoc(in: self)
    let rSquareLoc = node.rightSquare.bridgedSourceLoc(in: self)
    return BridgedArrayTypeRepr.createParsed(
      self.ctx,
      base: elementType,
      leftSquareLoc: lSquareLoc,
      rightSquareLoc: rSquareLoc
    )
  }

  public func generate(_ node: DictionaryTypeSyntax) -> BridgedTypeRepr {
    let keyType = generate(node.key)
    let valueType = generate(node.value)
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

  public func generate(_ node: MetatypeTypeSyntax) -> BridgedTypeRepr {
    let baseType = generate(node.baseType)
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

  public func generate(_ node: ImplicitlyUnwrappedOptionalTypeSyntax) -> BridgedTypeRepr {
    let base = generate(node.wrappedType)
    let exclaimLoc = node.exclamationMark.bridgedSourceLoc(in: self)
    return BridgedImplicitlyUnwrappedOptionalTypeRepr.createParsed(
      self.ctx,
      base: base,
      exclaimLoc: exclaimLoc
    )
  }

  public func generate(_ node: OptionalTypeSyntax) -> BridgedTypeRepr {
    let base = generate(node.wrappedType)
    let questionLoc = node.questionMark.bridgedSourceLoc(in: self)
    return BridgedOptionalTypeRepr.createParsed(
      self.ctx,
      base: base,
      questionLoc: questionLoc
    )
  }

  public func generate(_ node: PackExpansionTypeSyntax) -> BridgedTypeRepr {
    let base = generate(node.repetitionPattern)
    let repeatLoc = node.repeatKeyword.bridgedSourceLoc(in: self)
    return BridgedPackExpansionTypeRepr.createParsed(
      self.ctx,
      base: base,
      repeatKeywordLoc: repeatLoc
    )
  }

  public func generate(_ node: TupleTypeSyntax) -> BridgedTypeRepr {
    BridgedTupleTypeRepr.createParsed(
      self.ctx,
      elements: self.generate(node.elements),
      leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
      rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
    )
  }

  public func generate(_ node: CompositionTypeSyntax) -> BridgedTypeRepr {
    assert(node.elements.count > 1)

    let types = node.elements.lazy.map {
      generate($0.type)
    }

    return BridgedCompositionTypeRepr.createParsed(
      self.ctx,
      types: types.bridgedArray(in: self),
      ampersandLoc: (node.elements.first?.ampersand).bridgedSourceLoc(in: self)
    )
  }

  public func generate(_ node: FunctionTypeSyntax) -> BridgedTypeRepr {
    BridgedFunctionTypeRepr.createParsed(
      self.ctx,
      // FIXME: Why does `FunctionTypeSyntax` not have a `TupleTypeSyntax` child?
      argsType: BridgedTupleTypeRepr.createParsed(
        self.ctx,
        elements: self.generate(node.parameters),
        leftParenLoc: node.leftParen.bridgedSourceLoc(in: self),
        rightParenLoc: node.rightParen.bridgedSourceLoc(in: self)
      ),
      asyncLoc: (node.effectSpecifiers?.asyncSpecifier).bridgedSourceLoc(in: self),
      throwsLoc: (node.effectSpecifiers?.throwsSpecifier).bridgedSourceLoc(in: self),
      thrownType: self.generate(node.effectSpecifiers?.thrownError?.type).asNullable,
      arrowLoc: node.returnClause.arrow.bridgedSourceLoc(in: self),
      resultType: generate(node.returnClause.type)
    )
  }

  public func generate(_ node: NamedOpaqueReturnTypeSyntax) -> BridgedTypeRepr {
    let baseTy = generate(node.type)
    return BridgedNamedOpaqueReturnTypeRepr.createParsed(self.ctx, base: baseTy)
  }

  public func generate(_ node: SomeOrAnyTypeSyntax) -> BridgedTypeRepr {
    let someOrAnyLoc = node.someOrAnySpecifier.bridgedSourceLoc(in: self)
    let baseTy = generate(node.constraint)
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
}

// MARK: - SpecifierTypeRepr/AttrubutedTypeRepr

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
  public func generate(_ node: AttributedTypeSyntax) -> BridgedTypeRepr {
    var type = generate(node.baseType)

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
  func generate(_ node: TupleTypeElementListSyntax) -> BridgedArrayRef {
    node.lazy.map { element in
      let (firstName, firstNameLoc) = element.firstName.bridgedIdentifierAndSourceLoc(in: self)
      let (secondName, secondNameLoc) = element.secondName.bridgedIdentifierAndSourceLoc(in: self)
      var type = generate(element.type)
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

@_cdecl("swift_ASTGen_buildTypeRepr")
@usableFromInline
func buildTypeRepr(
  diagEnginePtr: UnsafeMutableRawPointer,
  sourceFilePtr: UnsafeRawPointer,
  typeLocPtr: UnsafePointer<UInt8>,
  dc: UnsafeMutableRawPointer,
  ctx: UnsafeMutableRawPointer,
  endTypeLocPtr: UnsafeMutablePointer<UnsafePointer<UInt8>?>
) -> UnsafeMutableRawPointer? {
  let sourceFile = sourceFilePtr.bindMemory(
    to: ExportedSourceFile.self,
    capacity: 1
  )

  // Find the type syntax node.
  guard
    let typeSyntax = findSyntaxNodeInSourceFile(
      sourceFilePtr: sourceFilePtr,
      sourceLocationPtr: typeLocPtr,
      type: TypeSyntax.self,
      wantOutermost: true
    )
  else {
    // FIXME: Produce an error
    return nil
  }

  // Fill in the end location.
  endTypeLocPtr.pointee = sourceFile.pointee.buffer.baseAddress!.advanced(by: typeSyntax.endPosition.utf8Offset)

  // Convert the type syntax node.
  return ASTGenVisitor(
    diagnosticEngine: .init(raw: diagEnginePtr),
    sourceBuffer: sourceFile.pointee.buffer,
    declContext: BridgedDeclContext(raw: dc),
    astContext: BridgedASTContext(raw: ctx)
  ).generate(typeSyntax).raw
}
