//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
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

public struct DistributedRequirementStubsMacro: DistributedMacro, DeclarationMacro {
  public static func expansion(of node: some FreestandingMacroExpansionSyntax, in context: some MacroExpansionContext) throws -> [DeclSyntax] {
    var decls: [DeclSyntax] = []
    decls.reserveCapacity(max(1, node.arguments.count - 2))

    // === Parameters
    var paramIdx = paramIndexInit()
    var typeParamIdx = typeParamIndexInit()

    // -- moduleName
    let moduleName = try takeModuleArgument(at: &paramIdx, node: node)
    // -- protocolName
    let protocolName = try takeProtocolNameArgument(at: &paramIdx, node: node)

    // -- stubProtocols: ["Name", "Other"]
    let stubProtocols = try takeStubProtocols(at: &paramIdx, node: node)
    for stubProtocol in stubProtocols {
      decls.append("#\(raw: stubProtocol)")
    }

    // -- collect the trailing requirements which are varargs
    for node in node.arguments.dropFirst(paramIdx) {
      let labeledExpr = node.cast(LabeledExprSyntax.self)
      for text in labeledExpr.children(viewMode: .all) {
        guard let text = text.as(StringLiteralExprSyntax.self) else {
          continue
        }
        for segment in text.segments {
          let decl: DeclSyntax = "\(raw: segment) { Swift.fatalError() }"
          decls.append(decl)
        }
      }
    }

    return decls
  }
}
