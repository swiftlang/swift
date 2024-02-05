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
    for node in node.attributes {
      switch node {
      case .attribute(let node):
        guard let attr = self.generateTypeAttribute(attribute: node) else {
          continue
        }
        attrs.add(attr);
      case .ifConfigDecl:
        fatalError("unimplemented")
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
        .escaping,
        .noEscape,
        .noDerivative,
        .async,
        .sendable,
        .retroactive,
        .unchecked,
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
        fatalError("unimplemented")
      case .opaqueReturnTypeOf:
        fatalError("unimplemented")

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
}
