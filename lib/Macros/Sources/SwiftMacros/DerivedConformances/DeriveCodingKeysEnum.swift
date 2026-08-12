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

/// Represents the information needed to synthesize one of the `CodingKey`
/// enumerations nested in a type deriving `Encodable` or `Decodable`.
public struct CodingKeysEnumInfo: TypeInfoProtocol {
  /// The name of the enum to synthesize, escaped as it must appear in Swift
  /// source.
  var name: String

  /// The name of each of its cases, escaped as they must appear in Swift
  /// source.
  var keys: [String]

  public static func fromSyntax(node: ExprSyntax) throws -> Self {
    // Expecting:
    //   CodingKeysEnumInfo(name: <String>, keys: <[String]>)

    let (name, keys) = try getNamedFuncallArgs(
      node: node,
      name: "CodingKeysEnumInfo"
    ).expect(
      .stringArg("name"),
      .stringArg("keys").toArray()
    )

    return Self(name: name, keys: keys)
  }

  public var syntax: ExprSyntax {
    """
    CodingKeysEnumInfo(name: \(stringlit(name)), keys: \(arraySyntax(keys, stringlit)))
    """
  }
}

public struct DeriveCodingKeysEnumMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let info = try node.arguments.expect(
      .init(parser: CodingKeysEnumInfo.fromStringLit)
    )

    let cases = info.keys.map { "case \($0)" }.joined(separator: "\n")

    return [
      """
      private enum \(raw: info.name): Swift::CodingKey {
        \(raw: cases)
      }
      """
    ]
  }
}
