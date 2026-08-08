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

/// Witness to derive for the `CaseIterable` protocol.
enum CaseIterableWitness: TypeInfoProtocol {

  /// The `AllCases` typealias
  case typeAlias

  /// The `allCases` var
  case varDef

  /// Parsing utility
  static func fromSyntax(node: ExprSyntax) throws -> CaseIterableWitness {
    switch node.trimmedDescription {
    case "typeAlias": .typeAlias
    case "varDef": .varDef
    default:
      fatalError(
        "Expected `typeAlias` or `varDef` but got \(node.trimmedDescription)"
      )
    }
  }

  var syntax: ExprSyntax {
    switch self {
    case .typeAlias: "typeAlias"
    case .varDef: "varDef"
    }
  }
}

public struct DeriveCaseIterableMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    // Expecting #_deriveCaseIterable(<type info>, <witness>)

    let (typeInfo, witnessKind) = try node.arguments.expect(
      .init(parser: NominalTypeInfo.fromStringLit),
      .init(parser: CaseIterableWitness.fromStringLit)
    )
    let enumInfo: EnumTypeInfo

    switch typeInfo.kind {
    case .enumLike(let info):
      enumInfo = info
    default: fatalError("Expected an enum")
    }

    // Derive the appropriate witness for the `witnessKind` requirement
    let witness: DeclSyntax

    switch witnessKind {
    case .typeAlias:
      witness =
        """
        typealias AllCases = [Self]
        """
    case .varDef:
      witness = expandVarDef(enumInfo)
    }

    return [witness]
  }

  /// Expands the `allCase` var declaration
  static func expandVarDef(_ enumInfo: EnumTypeInfo) -> DeclSyntax {
    """
      nonisolated static var allCases: [Self] {
        return [\(raw: enumInfo.cases.map { ".\($0.name)" }.joined(separator: ", "))]
      }
    """
  }
}
