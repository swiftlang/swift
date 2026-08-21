// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath
// RUN: %sourcekitd-test -req=collect-var-type %t/test.swift -- -swift-version 5 -load-plugin-library %t/%target-library-name(MacroPlugin) -module-name MacroUser %t/test.swift | %FileCheck %s

// CHECK: (12:9, 12:14): Int (explicit type: 0)

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct EmitClosureArrayMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    [
      """
      static var generated: [Int] {
        [accept { value in value }]
      }
      """
    ]
  }
}

//--- test.swift
func accept(_ transform: (Int) -> Int) -> Int {
  transform(0)
}

@attached(member, names: named(generated))
macro EmitClosureArray() =
  #externalMacro(module: "MacroPlugin", type: "EmitClosureArrayMacro")

@EmitClosureArray
struct Demo {
  func triggerInlayHints() {
    let local = 0
    print(local)
  }
}
