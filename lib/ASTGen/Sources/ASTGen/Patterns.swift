//===--- Patterns.swift ---------------------------------------------------===//
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

extension ASTGenVisitor {
  func generate(pattern node: PatternSyntax) -> BridgedPattern {
    switch node.as(PatternSyntaxEnum.self) {
    case .expressionPattern:
      break
    case .identifierPattern(let node):
      return self.generate(identifierPattern: node).asPattern
    case .isTypePattern:
      break
    case .missingPattern:
      break
    case .tuplePattern:
      break
    case .valueBindingPattern:
      break
    case .wildcardPattern:
      break
    }

    preconditionFailure("unimplemented")
  }

  func generate(identifierPattern node: IdentifierPatternSyntax) -> BridgedNamedPattern {
    let (name, nameLoc) = self.generateIdentifierAndSourceLoc(node.identifier)
    return .createParsed(
      ctx, declContext: declContext,
      name: name,
      loc: nameLoc
    )
  }
}


