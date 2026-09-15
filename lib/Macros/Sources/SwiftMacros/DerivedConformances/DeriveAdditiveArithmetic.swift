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

enum Operator: TypeInfoProtocol {
  case add
  case sub

  var symbol: String {
    switch self {
    case .add: return "+"
    case .sub: return "-"
    }
  }

  static func fromSyntax(node: ExprSyntax) throws -> Self {
    switch node.trimmedDescription {
    case "+", "add":
      return .add
    case "-", "sub":
      return .sub
    default:
      fatalError("Expected `+` or `-` but got `\(node.trimmedDescription)`")
    }
  }

  var syntax: ExprSyntax {
    switch self {
    case .add:
      return "+"
    case .sub:
      return "-"
    }
  }

}

enum ArithRequirement: TypeInfoProtocol {
  case op(Operator)
  case zero

  static func fromSyntax(node: ExprSyntax) throws -> Self {
    if node.trimmedDescription == "zero" {
      return .zero
    }
    let op = try getNamedFuncallArgs(
      node: node,
      name: "op"
    )
    .expect(.init(parser: Operator.fromSyntax))
    return .op(op)
  }

  var syntax: ExprSyntax {
    switch self {
    case .op(let o):
      return "op(\(raw: o.syntax))"
    case .zero:
      return "zero"
    }
  }

  func memberExpr(member: StoredProperty) -> String {
    switch self {
    case .op(let o):
      return "lhs.\(member.name) \(o.symbol) rhs.\(member.name)"
    case .zero:
      return "\(member.typeName).zero"
    }
  }

  func wrapBody(body: ExprSyntax) -> DeclSyntax {
    switch self {
    case .op(let op):
      return """
        static func \(raw: op.symbol)(_ lhs: Self, _ rhs: Self) -> Self {
          return \(body)
        }
        """
    case .zero:
      return
        """
        static var zero: Self {
          \(body)
        } 
        """
    }
  }
}

public struct DeriveAdditiveArithmeticMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let (typeInfo, req) = try node.arguments.expect(
      .init(parser: NominalTypeInfo.fromStringLit), .init(parser: ArithRequirement.fromStringLit))
    let structInfo: StructTypeInfo
    switch typeInfo.kind {
    case .structLike(let info):
      structInfo = info
    default: fatalError("Expected an enum")
    }

    return [req.wrapBody(body: getInit(structInfo, req: req))]
  }

  static func getInit(_ structInfo: StructTypeInfo, req: ArithRequirement) -> ExprSyntax {
    """
    Self(\(raw: structInfo.properties.map { p in
      "\(p.name): \(req.memberExpr(member: p))"
    }.joined(separator: ", ")))
    """
  }
}
