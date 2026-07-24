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

enum NSErrorDomainKind: TypeInfoProtocol {
  case objectiveC(domain: String)
  case regular

  static func fromSyntax(node: ExprSyntax) throws -> NSErrorDomainKind {
    if node.is(FunctionCallExprSyntax.self) {
      let args = try getNamedFuncallArgs(node: node, name: "objectiveC")
      let domain = try args.expect(
        .stringArg("domain")
      )
      return .objectiveC(domain: domain)
    }
    guard node.trimmedDescription == "regular" else {
      fatalError("Expected `objectiveC(...)` or `regular` cases")
    }
    return .regular
  }

  var syntax: SwiftSyntax.ExprSyntax {
    switch self {
    case .objectiveC(let domain):
      """
      objectiveC(domain: \(stringlit(domain)))
      """
    case .regular:
      """
      regular
      """
    }
  }

  func expandValue() -> ExprSyntax {
    switch self {
    case .objectiveC(let domain):
      stringlit(domain)
    case .regular:
      """
      String(reflecting: self)
      """
    }
  }

  func expand() -> DeclSyntax {
    """
    static var _nsErrorDomain: String {
      return \(self.expandValue())
    }
    """
  }
}

public struct DeriveErrorMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax, in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let kind = try node.arguments.expect(
      .init(parser: NSErrorDomainKind.fromStringLit))
    return [kind.expand()]
  }
}
