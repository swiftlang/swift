// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %sourcekitd-test -req=semantic-tokens %t/main.swift -- %t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) > %t/result.response
// RUN: diff -u %t/expected.response %t/result.response

// REQUIRES: swift_swift_parser

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct LogMacro: BodyMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
        in context: some MacroExpansionContext,
    ) throws -> [CodeBlockItemSyntax] {
        return ["print()"]
    }
}
//--- main.swift
@attached(body)
public macro log() = #externalMacro(module: "MacroDefinition", type: "LogMacro")

// Make sure we don't walk into the macro expanded body here.
@log func foo() {}

//--- expected.response
{
  key.semantic_tokens: [
    {
      key.kind: source.lang.swift.ref.macro,
      key.offset: 161,
      key.length: 3
    }
  ]
}
