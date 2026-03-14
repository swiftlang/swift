//===--- ASTGen+CompilerBuildConfiguration.swift --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftDiagnostics
import SwiftIfConfig
import SwiftSyntax

/// Enumeration that separates an #if config decl from the underlying element.
enum IfConfigOrUnderlying<Element> {
  case ifConfigDecl(IfConfigDeclSyntax)
  case underlying(Element)
}

extension ASTGenVisitor {
  /// Determine the active clause for the given #if, emitting any diagnostics
  /// produced due to the evaluation.
  func activeClause(in node: IfConfigDeclSyntax) -> IfConfigClauseSyntax? {
    // Determine the active clause.
    return configuredRegions.activeClause(for: node)
  }

  /// Visit a collection of elements that may contain #if clauses within it
  /// within it, recursing into the active clauses and to ensure that every
  /// active element is visited.
  /// - Parameters:
  ///   - elements: A syntax collection that can config `IfConfigDeclSyntax`
  ///     nodes in it.
  ///   - flatElementType: The element type within the syntax collection for the
  ///     non-#if case.
  ///   - split: Splits an element in elements into the two cases: #if config
  ///     or a flat element.
  ///   - body: The body that handles each syntax node of type FlatElement.
  func visitIfConfigElements<Elements, FlatElement>(
    _ elements: Elements,
    of flatElementType: FlatElement.Type,
    split: (Elements.Element) -> IfConfigOrUnderlying<FlatElement>,
    body: (FlatElement) -> Void
  ) where Elements: SyntaxCollection, FlatElement: SyntaxProtocol {
    for element in elements {
      switch split(element) {
      case .ifConfigDecl(let ifConfigDecl):
        if let activeClause = self.activeClause(in: ifConfigDecl),
           let activeElements = activeClause.elements {
          guard let activeElements = activeElements._syntaxNode.as(Elements.self) else {
            fatalError("Parser produced invalid nesting of #if decls")
          }

          visitIfConfigElements(
            activeElements,
            of: FlatElement.self,
            split: split,
            body: body
          )
        }

      case .underlying(let element):
        body(element)
    }
    }
  }
}


