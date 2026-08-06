// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %t/macro_expand_nested_lexical_context.swift -disable-availability-checking

// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %t/macro_expand_nested_lexical_context.swift -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

/// Peer macro that reports the lexical context it was expanded in, so that
/// the test can check the context macros observe.
public struct ReportLexicalContextMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let description = context.lexicalContext.map { "\($0.kind)" }.joined(separator: ",")
    let name = context.makeUniqueName("lexicalContext")
    return [
      """
      var \(name): String { "lexicalContext(\(raw: description))" }
      """
    ]
  }
}

/// Peer macro that introduces a declaration annotated with
/// `@ReportLexicalContext`.
public struct NestReportInPeerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      @ReportLexicalContext
      func \(context.makeUniqueName("nested"))() {}
      """
    ]
  }
}

/// Peer macro that introduces a declaration annotated with `@NestReportInPeer`,
/// so that the reporting macro ends up two expansions deep.
public struct NestReportInPeerTwiceMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      @NestReportInPeer
      func \(context.makeUniqueName("nestedTwice"))() {}
      """
    ]
  }
}

/// Member macro that introduces a member annotated with
/// `@ReportLexicalContext`.
public struct NestReportInMemberMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      @ReportLexicalContext
      func \(context.makeUniqueName("nestedMember"))() {}
      """
    ]
  }
}

/// Freestanding declaration macro that introduces a declaration annotated with
/// `@ReportLexicalContext`.
public struct NestReportInFreestandingMacro: DeclarationMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
      """
      @ReportLexicalContext
      func \(context.makeUniqueName("nestedFreestanding"))() {}
      """
    ]
  }
}

//--- macro_expand_nested_lexical_context.swift

@attached(peer)
macro ReportLexicalContext() = #externalMacro(module: "MacroDefinition", type: "ReportLexicalContextMacro")

@attached(peer)
macro NestReportInPeer() = #externalMacro(module: "MacroDefinition", type: "NestReportInPeerMacro")

@attached(peer)
macro NestReportInPeerTwice() = #externalMacro(module: "MacroDefinition", type: "NestReportInPeerTwiceMacro")

@attached(member)
macro NestReportInMember() = #externalMacro(module: "MacroDefinition", type: "NestReportInMemberMacro")

@freestanding(declaration)
macro nestReportInFreestanding() = #externalMacro(module: "MacroDefinition", type: "NestReportInFreestandingMacro")

// A macro expanded inside another macro's expansion must observe the same
// lexical context it would have observed had it been written in the original
// source, rather than an empty context. The syntax tree of an expansion buffer
// stops at the buffer, so this requires stitching the context back together
// across buffers.

struct S {
  // Written directly, as a baseline.
  @ReportLexicalContext
  func direct() {}

  // Reached through a peer macro's expansion.
  @NestReportInPeer
  func viaPeer() {}

  // Reached through two levels of peer macro expansion.
  @NestReportInPeerTwice
  func viaPeerTwice() {}

  // Reached through a freestanding macro's expansion.
  #nestReportInFreestanding
}

// Reached through a member macro's expansion.
@NestReportInMember
struct T {}

// Every one of the above sits inside a struct, so all of them must report the
// struct as their lexical context.
// CHECK-COUNT-5: lexicalContext(structDecl)
// CHECK-NOT: lexicalContext(structDecl)

// At file scope there is no enclosing declaration, so the context is empty
// whether or not the macro is nested.
@ReportLexicalContext
func topLevelDirect() {}

@NestReportInPeer
func topLevelViaPeer() {}

// CHECK-COUNT-2: lexicalContext()
// CHECK-NOT: lexicalContext()
