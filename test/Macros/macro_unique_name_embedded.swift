// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Create the plugin
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroPlugin) -module-name TestModule %t/TestModule.swift \
// RUN:   -enable-experimental-feature Embedded -wmo -target arm64-apple-macos14

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct AddMemberMacro: PeerMacro {
  public static func expansion(of node: AttributeSyntax, providingPeersOf declaration: some DeclSyntaxProtocol, in context: some MacroExpansionContext) throws -> [DeclSyntax] {
    return [
      """
      func \(context.makeUniqueName("foo"))() { }
      """
    ]
  }
}

//--- TestModule.swift
@attached(peer)
public macro AddMember() = #externalMacro(module: "MacroPlugin", type: "AddMemberMacro")

@AddMember
struct TestStruct { }
