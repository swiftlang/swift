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
  func generateTypeAttributes(_ node: some WithAttributesSyntax) -> BridgedTypeAttributes? {
    guard !node.attributes.isEmpty else {
      return nil
    }

    let attrs = BridgedTypeAttributes.new()
    visitIfConfigElements(node.attributes, of: AttributeSyntax.self) { element in
      switch element {
      case .ifConfigDecl(let ifConfigDecl):
        return .ifConfigDecl(ifConfigDecl)
      case .attribute(let attribute):
        return .underlying(attribute)
      }
    } body: { attribute in
      if let attr = self.generateTypeAttribute(attribute: attribute) {
        attrs.add(attr)
      }
    }

    guard !attrs.isEmpty else {
      attrs.delete()
      return nil
    }

    return attrs
  }

  func generateTypeAttribute(attribute node: AttributeSyntax) -> BridgedTypeAttribute? {
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

      case .opened:
        fatalError("unimplemented")
      case .packElement:
        fatalError("unimplemented")
      case .differentiable:
        fatalError("unimplemented")
      case .convention:
        return self.generateConventionTypeAttr(attribute: node)?.asTypeAttribute
        fatalError("unimplemented")
      case .opaqueReturnTypeOf:
        fatalError("unimplemented")

      case .isolated:
        return self.generateIsolatedTypeAttr(attribute: node)?.asTypeAttribute

      case .execution:
        return self.generateExecutionTypeAttr(attribute: node)?.asTypeAttribute

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
        .out,
        .owned,
        .silIsolated,
        .silUnmanaged,
        .silUnowned,
        .silWeak,
        .silSending,
        .silImplicitLeadingParam,
        .unownedInnerPointer:
        break;

      // Not a type attribute.
      case .none:
        break;
      }
    }

    // TODO: Diagnose.
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
    // FIXME: This don't need custom attribute arguments syntax.
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
      parensRange: self.generateSourceRange(start: node.leftParen!, end: node.rightParen!),
      name: ctx.allocateCopy(string: args.conventionLabel.rawText.bridged),
      nameLoc: self.generateSourceLoc(args.conventionLabel),
      witnessMethodProtocol: witnessMethodProtocol,
      clangType: cTypeName ?? BridgedStringRef(),
      clangTypeLoc: cTypeNameLoc ?? BridgedSourceLoc()
    )
  }
  
  func generateIsolatedTypeAttr(attribute node: AttributeSyntax) -> BridgedIsolatedTypeAttr? {
    guard case .argumentList(let isolatedArgs) = node.arguments,
          isolatedArgs.count == 1,
          let labelArg = isolatedArgs.first,
          labelArg.label == nil,
          let isolationKindExpr = labelArg.expression.as(DeclReferenceExprSyntax.self),
          isolationKindExpr.argumentNames == nil
    else {
      // TODO: Diagnose.
      return nil
    }
  

    var isolationKind: BridgedIsolatedTypeAttrIsolationKind
    switch isolationKindExpr.baseName {
    case "any": isolationKind = .dynamicIsolation
    default:
      // TODO: Diagnose.
      return nil
    }

    return BridgedIsolatedTypeAttr.createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      lpLoc: self.generateSourceLoc(node.leftParen!),
      isolationKindLoc: self.generateSourceLoc(isolationKindExpr.baseName),
      isolationKind: isolationKind,
      rpLoc: self.generateSourceLoc(node.rightParen!)
    )
  }

  func generateExecutionTypeAttr(attribute node: AttributeSyntax) -> BridgedExecutionTypeAttr? {
    guard case .argumentList(let executionArgs) = node.arguments,
          executionArgs.count == 1,
          let labelArg = executionArgs.first,
          labelArg.label == nil,
          let behaviorExpr = labelArg.expression.as(DeclReferenceExprSyntax.self),
          behaviorExpr.argumentNames == nil
    else {
      // TODO: Diagnose.
      return nil
    }

    var behavior: BridgedExecutionTypeAttrExecutionKind
    switch behaviorExpr.baseName {
    case "concurrent": behavior = .concurrent
    case "caller": behavior = .caller
    default:
      // TODO: Diagnose.
      return nil
    }

    return BridgedExecutionTypeAttr.createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      lpLoc: self.generateSourceLoc(node.leftParen!),
      behaviorLoc: self.generateSourceLoc(behaviorExpr.baseName),
      behavior: behavior,
      rpLoc: self.generateSourceLoc(node.rightParen!)
    )
  }
}
