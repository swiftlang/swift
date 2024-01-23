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

public struct DistributedMakeDistributedResolveStubMacroCall: DistributedMacro, ExpressionMacro {
  static let moduleName = "Distributed"

  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    // === Parameters
    var paramIdx = -1

    // -- systemName // FIXME: Remove this, this is the generics workaround
//    let systemName = try systemNameArgument(at: &paramIdx, node: node)

    // -- stubName
    paramIdx += 1
    guard let stubNameParam = node.arguments.dropFirst(paramIdx).first else {
      throw DiagnosticsError(syntax: node, message: "Missing 'stubName' argument", id: .missingArgument)
    }

    // -- module
    paramIdx += 1
    // -- protocolName
    paramIdx += 1

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

    // === Stub protocols

    // ....

    // === Generic type arguments
    guard let genericClause = node.genericArgumentClause else {
      throw DiagnosticsError(syntax: node, message: "Missing generic argument clause", id: .missingGenericArgument)
    }

    guard let systemTyArg = genericClause.arguments.dropFirst().first else {
      throw DiagnosticsError(syntax: node, message: "Missing generic type argument 'DAS', genericClause was: \(genericClause)", id: .missingGenericArgument)
    }
    let systemTypeName = systemTyArg

    return """
           #resolve_\(raw: actorTypeName)<\(raw: systemTypeName)>(
             stubName: "\(raw: uniqueStubName)", 
             id: \(idParam.expression),
             using: \(systemParam.expression)
           ) as! (any \(raw: actorTypeName)) 
           """
  }
}
