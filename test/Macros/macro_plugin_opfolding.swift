// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
// RUN: split-file %s %t

//== Build the plugin library
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %t/MacroDefinition.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -Rmacro-loading -verify-ignore-unknown \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift

//--- MacroDefinition.swift

import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

final class SequenceFinder: SyntaxVisitor {
  var foundSequenceExpr: Bool = false
  init() {
    super.init(viewMode: .sourceAccurate)
  }

  override func visit(_ node: SequenceExprSyntax) -> SyntaxVisitorContinueKind {
    foundSequenceExpr = true
    return .visitChildren
  }
}

func hasSequenceExpr(_ node: some SyntaxProtocol) -> Bool {
  let finder = SequenceFinder()
  finder.walk(node)
  return finder.foundSequenceExpr
}

public struct OpFoldingTestMacro {}

extension OpFoldingTestMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    precondition(!hasSequenceExpr(node))
    precondition(hasSequenceExpr(declaration))
    return []
  }
}

extension OpFoldingTestMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    precondition(!hasSequenceExpr(node))
    precondition(hasSequenceExpr(declaration))
    precondition(hasSequenceExpr(member))
    return []
  }
}

extension OpFoldingTestMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    precondition(!hasSequenceExpr(node))
    return "1"
  }
}

//--- test.swift

@attached(peer)
macro OpFoldingTest(_: Any) = #externalMacro(module: "MacroDefinition", type: "OpFoldingTestMacro")

@freestanding(expression)
macro OpFoldingTestExpr(_: Any) -> Int = #externalMacro(module: "MacroDefinition", type: "OpFoldingTestMacro")

var a = 1, b = 2

@OpFoldingTest(1 + a)
func peerTest() {}

@OpFoldingTest(1 + b)
struct S {
  var value =  a + b
  func member(foo: Int, bar: Int) { _ = foo + bar }
}

let valu = #OpFoldingTestExpr(a + b)
