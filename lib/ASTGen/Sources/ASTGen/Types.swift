import SwiftParser
import SwiftSyntax

import CASTBridging

extension ASTGenVisitor {
  public func visit(_ node: SimpleTypeIdentifierSyntax) -> UnsafeMutableRawPointer {
    let loc = self.base.advanced(by: node.position.utf8Offset).raw

    var text = node.name.text
    let id = text.withUTF8 { buf in
      return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
    }

    return SimpleIdentTypeRepr_create(ctx, loc, id)
  }

  public func visit(_ node: MemberTypeIdentifierSyntax) -> UnsafeMutableRawPointer {
    var path = [(TokenSyntax, GenericArgumentClauseSyntax?)]()
    var memberRef: Syntax? = Syntax(node)
    while let nestedMember = memberRef?.as(MemberTypeIdentifierSyntax.self) {
      path.append((nestedMember.name, nestedMember.genericArgumentClause))
      memberRef = Syntax(nestedMember.baseType)
    }

    if let base = memberRef?.as(SimpleTypeIdentifierSyntax.self) {
      path.append((base.name, base.genericArgumentClause))
    }

    var elements = [UnsafeMutableRawPointer]()
    for (pathElement, generics) in path.reversed() {
      var nameText = pathElement.text
      let name = nameText.withUTF8 { buf in
        return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
      let nameLoc = self.base.advanced(by: pathElement.position.utf8Offset).raw

      if let generics = generics {
        let lAngle = self.base.advanced(by: generics.leftAngleBracket.position.utf8Offset).raw
        let rAngle = self.base.advanced(by: generics.rightAngleBracket.position.utf8Offset).raw
        elements.append(generics.arguments.map({ self.visit($0.argumentType) }).withBridgedArrayRef { genericArgs in
          GenericIdentTypeRepr_create(self.ctx, name, nameLoc, genericArgs, lAngle, rAngle)
        })
      } else {
        elements.append(SimpleIdentTypeRepr_create(self.ctx, nameLoc, name))
      }
    }

    return elements.withBridgedArrayRef { elements in
      return IdentTypeRepr_create(self.ctx, elements)
    }
  }

  public func visit(_ node: ArrayTypeSyntax) -> UnsafeMutableRawPointer {
    let elementType = visit(node.elementType)
    let lSquareLoc = self.base.advanced(by: node.leftSquareBracket.position.utf8Offset).raw
    let rSquareLoc = self.base.advanced(by: node.rightSquareBracket.position.utf8Offset).raw
    return ArrayTypeRepr_create(self.ctx, elementType, lSquareLoc, rSquareLoc)
  }

  public func visit(_ node: DictionaryTypeSyntax) -> UnsafeMutableRawPointer {
    let keyType = visit(node.keyType)
    let valueType = visit(node.valueType)
    let colonLoc = self.base.advanced(by: node.colon.position.utf8Offset).raw
    let lSquareLoc = self.base.advanced(by: node.leftSquareBracket.position.utf8Offset).raw
    let rSquareLoc = self.base.advanced(by: node.rightSquareBracket.position.utf8Offset).raw
    return DictionaryTypeRepr_create(self.ctx, keyType, valueType, colonLoc, lSquareLoc, rSquareLoc)
  }

  public func visit(_ node: MetatypeTypeSyntax) -> UnsafeMutableRawPointer {
    let baseType = visit(node.baseType)
    let tyLoc = self.base.advanced(by: node.typeOrProtocol.position.utf8Offset).raw
    if node.typeOrProtocol.text == "Type" {
      return MetatypeTypeRepr_create(self.ctx, baseType, tyLoc)
    } else {
      assert(node.typeOrProtocol.text == "Protocol")
      return ProtocolTypeRepr_create(self.ctx, baseType, tyLoc)
    }
  }

  public func visit(_ node: ImplicitlyUnwrappedOptionalTypeSyntax) -> UnsafeMutableRawPointer {
    let base = visit(node.wrappedType)
    let exclaimLoc = self.base.advanced(by: node.exclamationMark.position.utf8Offset).raw
    return ImplicitlyUnwrappedOptionalTypeRepr_create(self.ctx, base, exclaimLoc)
  }

  public func visit(_ node: OptionalTypeSyntax) -> UnsafeMutableRawPointer {
    let base = visit(node.wrappedType)
    let questionLoc = self.base.advanced(by: node.questionMark.position.utf8Offset).raw
    return OptionalTypeRepr_create(self.ctx, base, questionLoc)
  }

  public func visit(_ node: PackExpansionTypeSyntax) -> UnsafeMutableRawPointer {
    let base = visit(node.patternType)
    let ellipsisLoc = self.base.advanced(by: node.ellipsis.position.utf8Offset).raw
    return PackExpansionTypeRepr_create(self.ctx, base, ellipsisLoc)
  }

  public func visit(_ node: TupleTypeSyntax) -> UnsafeMutableRawPointer {
    return self.withBridgedTupleElements(node.elements) { elements in
      let lParenLoc = self.base.advanced(by: node.leftParen.position.utf8Offset).raw
      let rParenLoc = self.base.advanced(by: node.rightParen.position.utf8Offset).raw
      return TupleTypeRepr_create(self.ctx, elements, lParenLoc, rParenLoc)
    }
  }

  public func visit(_ node: CompositionTypeSyntax) -> UnsafeMutableRawPointer {
    assert(node.elements.count > 1)
    let types = node.elements.map { visit($0.type) }
    let firstTypeLoc = self.base.advanced(by: node.elements.first!.type.position.utf8Offset).raw
    return types.withBridgedArrayRef { types in
      return CompositionTypeRepr_create(self.ctx, types, firstTypeLoc)
    }
  }
  
  public func visit(_ node: FunctionTypeSyntax) -> UnsafeMutableRawPointer {
    return self.withBridgedTupleElements(node.arguments) { elements in
      let lParenLoc = self.base.advanced(by: node.leftParen.position.utf8Offset).raw
      let rParenLoc = self.base.advanced(by: node.rightParen.position.utf8Offset).raw
      let args = TupleTypeRepr_create(self.ctx, elements, lParenLoc, rParenLoc)
      let asyncLoc = node.asyncKeyword.map { self.base.advanced(by: $0.position.utf8Offset).raw }
      let throwsLoc = node.throwsOrRethrowsKeyword.map { self.base.advanced(by: $0.position.utf8Offset).raw }
      let arrowLoc = self.base.advanced(by: node.arrow.position.utf8Offset).raw
      let retTy = visit(node.returnType)
      return FunctionTypeRepr_create(self.ctx, args, asyncLoc, throwsLoc, arrowLoc, retTy)
    }
  }

  public func visit(_ node: NamedOpaqueReturnTypeSyntax) -> UnsafeMutableRawPointer {
    let baseTy = visit(node.baseType)
    return NamedOpaqueReturnTypeRepr_create(self.ctx, baseTy)
  }

  public func visit(_ node: ConstrainedSugarTypeSyntax) -> UnsafeMutableRawPointer {
    let someOrAnyLoc = self.base.advanced(by: node.someOrAnySpecifier.position.utf8Offset).raw
    let baseTy = visit(node.baseType)
    if node.someOrAnySpecifier.text == "some" {
      return OpaqueReturnTypeRepr_create(self.ctx, someOrAnyLoc, baseTy)
    } else {
      assert(node.someOrAnySpecifier.text == "any")
      return ExistentialTypeRepr_create(self.ctx, someOrAnyLoc, baseTy)
    }
  }

  public func visit(_ node: AttributedTypeSyntax) -> UnsafeMutableRawPointer {
    // FIXME: Respect the attributes
    return visit(node.baseType)
  }
}

extension ASTGenVisitor {
  private func withBridgedTupleElements<T>(
    _ elementList: TupleTypeElementListSyntax,
    action: (BridgedArrayRef) -> T
  ) -> T {
    var elements = [BridgedTupleTypeElement]()
    for element in elementList {
      var nameText = element.name?.text
      let name = nameText?.withUTF8 { buf in
        return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
      let nameLoc = element.name.map { self.base.advanced(by: $0.position.utf8Offset).raw }
      var secondNameText = element.secondName?.text
      let secondName = secondNameText?.withUTF8 { buf in
        return SwiftASTContext_getIdentifier(ctx, buf.baseAddress, buf.count)
      }
      let secondNameLoc = element.secondName.map { self.base.advanced(by: $0.position.utf8Offset).raw }
      let colonLoc = element.colon.map { self.base.advanced(by: $0.position.utf8Offset).raw }
      let type = visit(element.type)
      let trailingCommaLoc = element.trailingComma.map { self.base.advanced(by: $0.position.utf8Offset).raw }

      elements.append(BridgedTupleTypeElement(
        Name: name,
        NameLoc: nameLoc,
        SecondName: secondName,
        SecondNameLoc: secondNameLoc,
        UnderscoreLoc: nil, /*N.B. Only important for SIL*/
        ColonLoc: colonLoc,
        Type: type,
        TrailingCommaLoc: trailingCommaLoc))
    }
    return elements.withBridgedArrayRef { elements in
      return action(elements)
    }
  }
}
