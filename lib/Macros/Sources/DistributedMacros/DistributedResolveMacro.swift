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

public struct DistributedResolveMacro: DistributedMacro, ExpressionMacro {
  static let moduleName = "Distributed"

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    // === Parameters
    var paramIdx = paramIndexInit()
    var typeParamIdx = typeParamIndexInit()

    // -- id
    let idParam = try takeIdArgument(at: &paramIdx, node: node)
    // -- system
    let systemParam = try takeSystemArgument(at: &paramIdx, node: node)

    // === Generic type arguments
    guard let genericClause = node.genericArgumentClause else {
      throw DiagnosticsError(syntax: node, message: "Missing generic argument clause", id: .missingGenericArgument)
    }
    guard let actorTyArg = genericClause.arguments.first else {
      throw DiagnosticsError(syntax: node, message: "Missing generic type argument 'DA', genericClause was: \(genericClause)", id: .missingGenericArgument)
    }
    let actorTypeName: String =
      if let someOrAnyActorTy = actorTyArg.as(GenericArgumentSyntax.self) {
        String("\(someOrAnyActorTy.argument)".dropFirst("any ".count)) // FIXME: hacky, destructure it properly
      } else {
        fatalError("Not implemented")
      }

    guard let systemTyArg = genericClause.arguments.dropFirst().first else {
      throw DiagnosticsError(syntax: node, message: "Missing generic type argument 'DAS', genericClause was: \(genericClause)", id: .missingGenericArgument)
    }
    let systemTypeName = systemTyArg

    let uniqueStubName = context.makeUniqueName(actorTypeName)

    return """
           #resolve_\(raw: actorTypeName)<\(raw: systemTypeName)>(
             stubName: "\(raw: uniqueStubName)", 
             id: \(idParam.expression),
             using: \(systemParam.expression)
           ) as! (any \(raw: actorTypeName)) 
           """
  }
}
