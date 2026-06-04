// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the macro plugin.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %t/test.swift

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

/// Attaches `@available(*, unavailable)` to every member of the annotated type.
public struct UnavailableMembersMacro: MemberAttributeMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo decl: some DeclGroupSyntax,
    providingAttributesFor member: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [AttributeSyntax] {
    ["@available(*, unavailable, message: \"unavailable via macro\")"]
  }
}

//--- test.swift
@attached(memberAttribute)
macro UnavailableMembers() = #externalMacro(module: "MacroDefinition", type: "UnavailableMembersMacro")

@UnavailableMembers
class Base {
  func member() {} // expected-note 2 {{'member()' has been explicitly marked unavailable here}}
}

class Sub: Base {
  override func member() {} // expected-error {{cannot override 'member' which has been marked unavailable: unavailable via macro}}
  // expected-note@-1 {{remove 'override' modifier to declare a new 'member'}}
}

func useBaseMember(_ b: Base) {
  b.member() // expected-error {{'member()' is unavailable: unavailable via macro}}
}
