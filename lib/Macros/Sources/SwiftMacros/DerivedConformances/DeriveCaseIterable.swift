//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct DeriveCaseIterableMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    // Expecting #_deriveCaseIterable(<type info>)

    let typeInfo = try node.arguments.expect(
      .init(parser: NominalTypeInfo.fromStringLit))

    let enumInfo: EnumTypeInfo
    switch typeInfo.kind {
    case .enumLike(let info):
      enumInfo = info
    default: fatalError("Expected an enum")
    }
    return [expand(enumInfo)]
  }

  /// Expands the `allCase` var declaration
  static func expand(_ enumInfo: EnumTypeInfo) -> DeclSyntax {
    """
      nonisolated static var allCases: [Self] {
        return [\(raw: enumInfo.cases.map { ".\($0.name)" }.joined(separator: ", "))]
      }
    """
  }
}
