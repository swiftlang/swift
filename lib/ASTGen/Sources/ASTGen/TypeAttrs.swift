//===--- TypeAttrs.swift --------------------------------------------------===//
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
      let attrKind: swift.TypeAttrKind?
      do {
        let bridgedOptional = BridgedOptionalTypeAttrKind(from: attrName.bridged)
        attrKind = if bridgedOptional.hasValue {
          bridgedOptional.value
        } else {
          nil
        }
      }

      switch attrKind {
      // Simple type attributes.
      case .Autoclosure,
        .Addressable,
        .Concurrent,
        .Escaping,
        .NoEscape,
        .NoDerivative,
        .Async,
        .Sendable,
        .Retroactive,
        .Unchecked,
        .Unsafe,
        .Preconcurrency,
        .Local,
        .NoMetadata,
        .Nonisolated,
        .PackGuaranteed,
        .PackInout,
        .PackOut,
        .PackOwned,
        .Pseudogeneric,
        .Yields,
        .YieldMany,
        .YieldOnce,
        .YieldOnce2,
        .Thin,
        .Thick,
        .Unimplementable:
        return self.generateSimpleTypeAttr(attribute: node, kind: attrKind!)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))

      case .Convention:
        return (self.generateConventionTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .Differentiable:
        return (self.generateDifferentiableTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .OpaqueReturnTypeOf:
        return (self.generateOpaqueReturnTypeOfTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))
      case .Isolated:
        return (self.generateIsolatedTypeAttr(attribute: node)?.asTypeAttribute)
          .map(BridgedTypeOrCustomAttr.typeAttr(_:))

      // SIL type attributes are not supported.
      case .Autoreleased,
        .BlockStorage,
        .Box,
        .CalleeGuaranteed,
        .CalleeOwned,
        .CapturesGenerics,
        .Direct,
        .DynamicSelf,
        .Error,
        .ErrorIndirect,
        .ErrorUnowned,
        .Guaranteed,
        .In,
        .InConstant,
        .InGuaranteed,
        .InCXX,
        .Inout,
        .InoutAliasable,
        .MoveOnly,
        .ObjCMetatype,
        .Opened,
        .Out,
        .Owned,
        .PackElement,
        .SILIsolated,
        .SILUnmanaged,
        .SILUnowned,
        .SILWeak,
        .SILSending,
        .SILImplicitLeadingParam,
        .UnownedInnerPointer:
        // TODO: Diagnose or fallback to CustomAttr?
        fatalError("SIL type attributes are not supported")
        break;

      case nil:
        // Not a builtin type attribute. Fall back to CustomAttr
        break;
      }
    }

    if let customAttr = self.generateCustomAttr(attribute: node) {
      return .customAttr(customAttr)
    }
    return nil
  }

  func generateSimpleTypeAttr(attribute node: AttributeSyntax, kind: swift.TypeAttrKind) -> BridgedTypeAttribute? {
    // TODO: Diagnose extraneous arguments.
    return BridgedTypeAttribute.createSimple(
      self.ctx,
      kind: kind,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName)
    )
  }

  /// E.g.
  ///   ```
  ///   @convention(c)
  ///   @convention(c, cType: "int (*)(int)")
  ///   ```
  func generateConventionTypeAttr(attribute node: AttributeSyntax) -> BridgedConventionTypeAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      let nameAndLoc: (name: _, loc: _)? =  self.generateConsumingPlainIdentifierAttrOption(args: &args) {
        (ctx.allocateCopy(string: $0.rawText.bridged), self.generateSourceLoc($0))
      }
      guard let nameAndLoc else {
        return nil
      }

      let cTypeNameLoc: BridgedSourceLoc?
      let cTypeName: BridgedStringRef?
      if !args.isEmpty {
        cTypeNameLoc = self.generateSourceLoc(args.first?.expression)
        cTypeName = self.generateConsumingSimpleStringLiteralAttrOption(args: &args, label: "cType")
        guard cTypeName != nil else {
          return nil
        }
      } else {
        cTypeNameLoc = nil
        cTypeName = nil
      }

      // `@convention(witness_method: <protocol-name>)` is for SIL only.
      let witnessMethodProtocol: BridgedDeclNameRef = BridgedDeclNameRef()

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        nameLoc: self.generateSourceLoc(node.attributeName),
        parensRange: self.generateAttrParensRange(attribute: node),
        name: nameAndLoc.name,
        nameLoc: nameAndLoc.loc,
        witnessMethodProtocol: witnessMethodProtocol,
        clangType: cTypeName ?? BridgedStringRef(),
        clangTypeLoc: cTypeNameLoc ?? BridgedSourceLoc()
      )
    }
  }

  func generateDifferentiableTypeAttr(attribute node: AttributeSyntax) -> BridgedDifferentiableTypeAttr? {
    let differentiability: BridgedDifferentiabilityKind
    let differentiabilityLoc: BridgedSourceLoc
    if let args = node.arguments {
      guard let args = args.as(DifferentiableAttributeArgumentsSyntax.self) else {
        fatalError("(compiler bug) invalid arguments for @differentiable attribute")
      }

      if let kindSpecifier = args.kindSpecifier {
        differentiability = self.generateDifferentiabilityKind(text: kindSpecifier.rawText)
        differentiabilityLoc = self.generateSourceLoc(kindSpecifier)

        guard differentiability != .nonDifferentiable else {
          // TODO: Diagnose
          fatalError("invalid kind for @differentiable type attribute")
        }

        guard kindSpecifier.nextToken(viewMode: .fixedUp) == node.rightParen else {
          // TODO: Diagnose
          fatalError("only expeceted 'reverse' in @differentiable type attribute")
        }
      } else {
        // TODO: Diagnose
        fatalError("expected @differentiable(reverse)")
      }
    } else {
      differentiability = .normal
      differentiabilityLoc = nil
    }

    // Only 'reverse' is formally supported today. '_linear' works for testing
    // purposes. '_forward' is rejected.
    switch differentiability {
    case .normal, .nonDifferentiable:
      // TODO: Diagnose
      fatalError("Only @differentiable(reverse) is supported")
    case .forward:
      // TODO: Diagnose
      fatalError("Only @differentiable(reverse) is supported")
    case .reverse, .linear:
      break
    }

    return .createParsed(
      self.ctx,
      atLoc: self.generateSourceLoc(node.atSign),
      nameLoc: self.generateSourceLoc(node.attributeName),
      parensRange: self.generateAttrParensRange(attribute: node),
      kind: differentiability,
      kindLoc: differentiabilityLoc
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

  /// E.g.
  ///   ```
  ///   @_opaqueReturnTypeOf("$sMangledName", 4)
  ///   ```
  func generateOpaqueReturnTypeOfTypeAttr(attribute node: AttributeSyntax) -> BridgedOpaqueReturnTypeOfTypeAttr? {
    self.generateWithLabeledExprListArguments(attribute: node) { args in
      let mangledLoc = self.generateSourceLoc(args.first?.expression)
      let mangledName = self.generateConsumingSimpleStringLiteralAttrOption(args: &args)
      guard let mangledName else {
        return nil
      }

      let indexLoc = self.generateSourceLoc(args.first?.expression)
      let index: Int? = self.generateConsumingAttrOption(args: &args, label: nil) { expr in
        guard let intExpr = expr.as(IntegerLiteralExprSyntax.self) else {
          // TODO: Diagnostics.
          fatalError("expected integer literal")
          // return nil
        }
        return intExpr.representedLiteralValue
      }
      guard let index else {
        return nil
      }

      return .createParsed(
        self.ctx,
        atLoc: self.generateSourceLoc(node.atSign),
        nameLoc: self.generateSourceLoc(node.attributeName),
        parensRange: self.generateAttrParensRange(attribute: node),
        mangled: mangledName,
        mangledLoc: mangledLoc,
        index: index,
        indexLoc: indexLoc
      )
    }

  }
  
  func generateAttrParensRange(attribute node: AttributeSyntax) -> BridgedSourceRange {
    guard let lParen = node.leftParen else {
      return BridgedSourceRange()
    }
    return self.generateSourceRange(start: lParen, end: node.lastToken(viewMode: .sourceAccurate)!)
  }
}
