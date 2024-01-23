//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import SwiftSyntax
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntaxBuilder

public struct DistributedResolveStubMacro: ExpressionMacro {
  static let moduleName = "Distributed"

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    // === Parameters
    var paramIdx = -1

    // -- stubName
    paramIdx += 1
    guard let stubNameParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'stubName' argument", id: .missingArgument)
    }
    guard let stubNameLiteral = stubNameParam.expression.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticsError(syntax: node, message: "Expected 'stubName' to be a string literal, was: \(stubNameParam.expression)", id: .illegalSyntaxNode)
    }
    guard let stubName = stubNameLiteral.segments.first else {
      throw DiagnosticsError(syntax: node, message: "Expected 'stubName' to be a string literal 1 segment, was: \(stubNameLiteral.debugDescription)", id: .illegalSyntaxNode)
    }

    // -- module
    paramIdx += 1
    guard let moduleParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'moduleName' argument", id: .missingArgument)
    }
    guard let moduleNameLiteral = moduleParam.expression.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticsError(syntax: node, message: "Expected 'moduleName' to be a string literal, was: \(moduleParam.expression)", id: .illegalSyntaxNode)
    }

    // -- protocol
    paramIdx += 1
    guard let protocolNameParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'protocolName' argument", id: .missingArgument)
    }
    guard let protocolNameLiteral = protocolNameParam.expression.as(StringLiteralExprSyntax.self) else {
      throw DiagnosticsError(syntax: node, message: "Expected 'protocolName' to be a string literal, was: \(protocolNameParam.expression)", id: .illegalSyntaxNode)
    }
    guard let protocolName = protocolNameLiteral.segments.first else {
      throw DiagnosticsError(syntax: node, message: "Expected 'protocolName' to be a string literal 1 segment, was: \(protocolNameLiteral.debugDescription)", id: .illegalSyntaxNode)
    }

    // -- systemName
//    paramIdx += 1
//    guard let systemNameParam = node.arguments.dropFirst(paramIdx).first else {
//      throw DiagnosticsError(syntax: node, message: "Missing 'systemName' argument", id: .missingArgument)
//    }
//    guard let systemNameLiteral = systemNameParam.expression.as(StringLiteralExprSyntax.self) else {
//      throw DiagnosticsError(syntax: node, message: "Expected 'systemName' to be a string literal, was: \(systemNameParam.expression)", id: .illegalSyntaxNode)
//    }
//    guard let systemName = systemNameLiteral.segments.first else {
//      throw DiagnosticsError(syntax: node, message: "Expected 'systemName' to be a string literal 1 segment, was: \(systemNameLiteral.debugDescription)", id: .illegalSyntaxNode)
//    }

    // -- id
    paramIdx += 1
    guard let idParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'id' argument", id: .missingArgument)
    }

    // -- system
    paramIdx += 1
    guard let systemParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'system' argument", id: .missingArgument)
    }

    // String representations of all the protocol requirements
    paramIdx += 1
    let protocolStubMacros: String =
      node.arguments.dropFirst(paramIdx)
        .map { literal in
          "#\(literal.expression.as(StringLiteralExprSyntax.self)!.segments.first!.trimmed)_stubs"
        }
        .joined(separator: "\n")

    // === Generic type arguments
    guard let genericClause = node.genericArgumentClause else {
      throw DiagnosticsError(syntax: node, message: "Missing generic argument clause", id: .missingGenericArgument)
    }
//    guard let actorTyArg = genericClause.arguments.first else {
//      throw DiagnosticsError(syntax: node, message: "Missing generic type argument 'DA', genericClause was: \(genericClause)", id: .missingGenericArgument)
//    }
//    print("actor = \(actorTyArg)")
//
    guard let systemTyArg = genericClause.arguments.first else {
      throw DiagnosticsError(syntax: node, message: "Missing generic type argument 'DAS', genericClause was: \(genericClause)", id: .missingGenericArgument)
    }
    let systemTypeName = systemTyArg

    let stubTypeName = "$\(protocolNameLiteral.segments.first!)"

//                 #distributedActorStubType<\(raw: systemName)>(
//               stubName: "\(raw: stubName)",
//               module: \(moduleNameLiteral), protocolName: \(protocolNameLiteral),
//               \(raw: protocolsToStub)
//             )

    return """
           {
             distributed actor \(raw: stubName): \(protocolName) {
               typealias ActorSystem = \(raw: systemTypeName)
               \(raw: protocolStubMacros)
             }
             return try \(raw: stubName).resolve(
               id: \(idParam.expression), 
               using: \(systemParam.expression)
             )
           }()
           """

    //                // stubs for every protocol
    //               // #\(protocolName)_stubs
  }
}
