// REQUIRES: swift_swift_parser, OS=macosx

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/src
// RUN: mkdir -p %t/plugins

// RUN: split-file %s %t/src

//#-- Prepare the macro dylib plugin.
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name MacroDefinition \
// RUN:   %t/src/MacroDefinition.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -enable-experimental-feature FreestandingMacros \
// RUN:   -parse-as-library \
// RUN:   -dump-macro-expansions \
// RUN:   -plugin-path %t/plugins \
// RUN:   %t/src/test.swift


//--- MacroDefinition.swift
import SwiftDiagnostics
import SwiftOperators
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct GlobalFuncAndVarMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["func globalFunc() {}", "var globalVar: Int { 1 }"]
  }
}

public struct MemberFuncAndVarMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["func memberFunc() {}", "var memberVar: Int { 1 }"]
  }
}

public struct LocalFuncAndVarMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return ["func LocalFunc() {}", "var LocalVar: Int { 1 }"]
  }
}

//--- test.swift

@freestanding(declaration, names: named(globalFunc), named(globalVar)) macro globalDecls() = #externalMacro(module: "MacroDefinition", type: "GlobalFuncAndVarMacro")
@freestanding(declaration, names: named(memberFunc), named(memberVar)) macro memberDecls() = #externalMacro(module: "MacroDefinition", type: "MemberFuncAndVarMacro")
@freestanding(declaration, names: named(localFunc), named(localVar)) macro localDecls() = #externalMacro(module: "MacroDefinition", type: "LocalFuncAndVarMacro")

@available(SwiftStdlib 9999, *)
#globalDecls

func testGlobal() { // expected-note {{add @available attribute to enclosing global function}}
  globalFunc() // expected-error {{'globalFunc()' is only available in macOS 9999 or newer}} expected-note {{add 'if #available' version check}}
  // FIXME(109376568): Global variable introduced by macro expansion not found
  _ = globalVar // expected-error {{cannot find 'globalVar' in scope}}
}

struct S {
  @available(SwiftStdlib 9999, *)
  #memberDecls
}
func testMember(value: S) { // expected-note 2 {{add @available attribute to enclosing global function}}
  value.memberFunc() // expected-error {{'memberFunc()' is only available in macOS 9999 or newer}} expected-note {{add 'if #available' version check}}
  _ = value.memberVar // expected-error {{'memberVar' is only available in macOS 9999 or newer}} expected-note {{add 'if #available' version check}}
}

struct T {
  static #memberDecls
}
func testStatic() {
  T.memberFunc() // OK
  _ = T.memberVar // OK
}

func testLocal() {
// FIXME(109376102): Local vars with freestanding macro is not supported yet.
#if false
  #localDecls
  do {
  }
#endif
}
