//===--- TypeAttrs.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
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

extension ASTGenVisitor {
  func generateTypeAttributes(_ node: some WithAttributesSyntax) -> [BridgedTypeOrCustomAttr] {
    var attrs: [BridgedTypeOrCustomAttr] = []
    visitIfConfigElements(node.attributes, of: AttributeSyntax.self) { element in
      switch element {
      case .ifConfigDecl(let ifConfigDecl):
        return .ifConfigDecl(ifConfigDecl)
      case .attribute(let attribute):
        return .underlying(attribute)
      }
    } body: { attribute in
      if let attr = self.generateTypeAttribute(attribute: attribute) {
        attrs.append(attr)
      }
    }
    return attrs
  }

  func generateTypeAttribute(attribute node: AttributeSyntax) -> BridgedTypeOrCustomAttr? {
    if let identTy = node.attributeName.as(IdentifierTypeSyntax.self) {
      let attrName = identTy.name.rawText
      let attrKind = BridgedTypeAttrKind(from: attrName.bridged)

      switch attrKind {
      // Simple type attributes.
      case .autoclosure,
        .addressable,
        .escaping,
        .noEscape,
        .noDerivative,
        .async,
        .sendable,
        .retroactive,
        .unchecked,
        .unsafe,
        .preconcurrency,
        .local,
        .noMetadata,
        .packGuaranteed,
        .packInout,
        .packOut,
        .packOwned,
        .pseudogeneric,
        .yields,
        .yieldMany,
        .yieldOnce,
        .yieldOnce2,
        .thin,
        .thick,
        .unimplementable:
        return self.generateSimpleTypeAttr(attribute: node, kind: attrKind)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))

      case .convention:
        return (self.generateConventionTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .differentiable:
        fatalError("unimplemented")
      case .execution:
        return (self.generateExecutionTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .opaqueReturnTypeOf:
        return (self.generateOpaqueReturnTypeOfTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .isolated:
        return (self.generateIsolatedTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))

      // SIL type attributes are not supported.
      case .autoreleased,
        .blockStorage,
        .box,
        .calleeGuaranteed,
        .calleeOwned,
        .capturesGenerics,
        .direct,
        .dynamicSelf,
        .error,
        .errorIndirect,
        .errorUnowned,
        .guaranteed,
        .in,
        .inConstant,
        .inGuaranteed,
        .inCXX,
        .inout,
        .inoutAliasable,
        .moveOnly,
        .objCMetatype,
        .opened,
        .out,
        .owned,
        .packElement,
        .silIsolated,
        .silUnmanaged,
        .silUnowned,
        .silWeak,
        .silSending,
        .silImplicitLeadingParam,
        .unownedInnerPointer:
        // TODO: Diagnose or fallback to CustomAttr?
        fatalError("SIL type attributes are not supported")
        break;

      case .none:
        // Not a builtin type attribute. Fall back to CustomAttr
        break;
      }
    }

    if let customAttr = self.generateCustomAttr(attribute: node) {
      return .customAttr(customAttr)
    }
    return nil
  }

  func generateSimpleTypeAttr(attribute node: AttributeSyntax, kind: BridgedTypeAttrKind) -> BridgedTypeAttribute? {
    // TODO: Diagnose extraneous arguments.
    return BridgedTypeAttribute.createSimple(
      self.ctx,
      kind: kind,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName)
    )
  }
  
  func generateConventionTypeAttr(attribute node: AttributeSyntax) -> BridgedConventionTypeAttr? {
    // FIXME: This doesn't need custom attribute arguments syntax.
    // FIXME: Support 'witness_method' argument.
    guard let args = node.arguments?.as(ConventionAttributeArgumentsSyntax.self) else {
      // TODO: Diangose.
      return nil
    }
    
    let cTypeName: BridgedStringRef?
    let cTypeNameLoc: BridgedSourceLoc?
    if let ctypeString = args.cTypeString {
      cTypeName = self.generateStringLiteralTextIfNotInterpolated(expr: ctypeString)
      cTypeNameLoc = cTypeName != nil ? self.generateSourceLoc(ctypeString) : nil
    } else {
      cTypeName = nil
      cTypeNameLoc = nil
    }
    
    let witnessMethodProtocol: BridgedDeclNameRef = BridgedDeclNameRef()
    
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      parensRange: self.generateAttrParensRange(attribute: node),
      name: ctx.allocateCopy(string: args.conventionLabel.rawText.bridged),
      nameLoc: self.generateSourceLoc(args.conventionLabel),
      witnessMethodProtocol: witnessMethodProtocol,
      clangType: cTypeName ?? BridgedStringRef(),
      clangTypeLoc: cTypeNameLoc ?? BridgedSourceLoc()
    )
  }
  
  func generateExecutionTypeAttr(attribute node: AttributeSyntax) -> BridgedExecutionTypeAttr? {
    let behaviorLoc = self.generateSourceLoc(node.arguments)
    let behavior: BridgedExecutionTypeAttrExecutionKind? = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "concurrent": return .concurrent
        case "caller": return .caller
        default:
          // TODO: Diagnose.
          return nil
        }
      }
    )
    guard let behavior else {
      return nil
    }
      
    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      parensRange: self.generateAttrParensRange(attribute: node),
      behavior: behavior,
      behaviorLoc: behaviorLoc
    )
  }
  
  func generateIsolatedTypeAttr(attribute node: AttributeSyntax) -> BridgedIsolatedTypeAttr? {
    let isolationKindLoc = self.generateSourceLoc(node.arguments)
    let isolationKind: BridgedIsolatedTypeAttrIsolationKind? = self.generateSingleAttrOption(
      attribute: node,
      {
        switch $0.rawText {
        case "any": return .dynamicIsolation
        default:
          // TODO: Diagnose.
          return nil
        }
      }
    )
    guard let isolationKind else {
      return nil
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      parensRange: self.generateAttrParensRange(attribute: node),
      isolationKind: isolationKind,
      isolationKindLoc: isolationKindLoc
    )
  }

  func generateOpaqueReturnTypeOfTypeAttr(attribute node: AttributeSyntax) -> BridgedOpaqueReturnTypeOfTypeAttr? {
    // FIXME: This doesn't need custom attribute arguments syntax.
    guard let args = node.arguments?.as(OpaqueReturnTypeOfAttributeArgumentsSyntax.self) else {
      // TODO: Diagnose
      fatalError("expected arguments for @_opaqueReturnTypeOfType type attribute")
    }

    let mangledLoc = self.generateSourceLoc(args.mangledName)
    guard let mangled = self.generateStringLiteralTextIfNotInterpolated(expr: args.mangledName) else {
      // TODO: Diagnose
      fatalError("expected string literal for @_opaqueReturnTypeOfType type attribute")
    }
    
    let indexLoc = self.generateSourceLoc(args.ordinal)
    let index =  Int(args.ordinal.text, radix: 10)
    guard let index else {
      // TODO: Diagnose
      fatalError("expected integer literal for @_opaqueReturnTypeOfType type attribute")
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      parensRange: self.generateAttrParensRange(attribute: node),
      mangled: mangled,
      mangledLoc: mangledLoc,
      index: index, indexLoc: indexLoc
    )
  }
  
  func generateAttrParensRange(attribute node: AttributeSyntax) -> BridgedSourceRange {
    guard let lParen = node.leftParen else {
      return BridgedSourceRange()
    }
    return self.generateSourceRange(start: lParen, end: node.lastToken(viewMode: .sourceAccurate)!)
  }
}
