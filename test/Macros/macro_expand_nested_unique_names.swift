// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %t/macro_expand_nested_unique_names.swift -disable-availability-checking

// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %t/macro_expand_nested_unique_names.swift -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck %s < %t/expansions-dump.txt

// Declarations introduced by a macro nested inside another macro's expansion
// must also be emitted, not just type-checked.
// RUN: %target-swift-emit-silgen -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library %t/macro_expand_nested_unique_names.swift -disable-availability-checking | %FileCheck %s -check-prefix=CHECK-SIL

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

/// Peer macro that introduces a declaration which is itself annotated with
/// the "inner" macro below.
public struct NestedMacroOuterMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let funcDecl = declaration.as(FunctionDeclSyntax.self) else {
      return []
    }

    let name = context.makeUniqueName(funcDecl.name.text)
    return [
      """
      @NestedMacroInner
      func \(name)() {}
      """
    ]
  }
}

/// Peer macro that introduces two declarations with unique names, where one
/// refers to the other.
public struct NestedMacroInnerMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let storageName = context.makeUniqueName("storage")
    let accessorName = context.makeUniqueName("accessor")
    return [
      """
      var \(storageName): Int { 42 }
      """,
      """
      func \(accessorName)() -> Int { return \(storageName) }
      """,
    ]
  }
}

//--- macro_expand_nested_unique_names.swift

@attached(peer)
macro NestedMacroOuter() = #externalMacro(module: "MacroDefinition", type: "NestedMacroOuterMacro")

@attached(peer)
macro NestedMacroInner() = #externalMacro(module: "MacroDefinition", type: "NestedMacroInnerMacro")

// A macro used directly, as a baseline: the unique names it introduces must
// resolve against each other.
@NestedMacroInner
func directlyAnnotated() {}

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names17directlyAnnotated16NestedMacroInnerfMp_.swift
// CHECK: var [[DIRECT_STORAGE:\$s32macro_expand_nested_unique_names17directlyAnnotated16NestedMacroInnerfMp_7storagefMu_]]: Int
// CHECK: func $s32macro_expand_nested_unique_names17directlyAnnotated16NestedMacroInnerfMp_8accessorfMu_() -> Int {
// CHECK:   return [[DIRECT_STORAGE]]

// The same inner macro, but reached through the expansion of another macro.
// The unique names it introduces must still resolve against each other, even
// though the declaration they are attached to lives in a macro expansion
// buffer rather than in the original source file.
@NestedMacroOuter
func topLevelOuter() {}

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names13topLevelOuter011NestedMacroH0fMp_.swift
// CHECK: @NestedMacroInner
// CHECK: func $s32macro_expand_nested_unique_names13topLevelOuter011NestedMacroH0fMp_13topLevelOuterfMu_()

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names010$s32macro_{{.*}}0jK5InnerfMp_.swift
// CHECK: var [[TOP_STORAGE:\$s32macro_expand_nested_unique_names010\$s32macro_.*7storagefMu_]]: Int
// CHECK: func $s32macro_expand_nested_unique_names010$s32macro_{{.*}}8accessorfMu_() -> Int {
// CHECK:   return [[TOP_STORAGE]]

// The same thing in a type context, which uses a different name lookup path.
struct S {
  @NestedMacroInner
  func directlyAnnotated() {}

  @NestedMacroOuter
  func nestedOuter() {}
}

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names1SV17directlyAnnotated16NestedMacroInnerfMp_.swift
// CHECK: var [[MEMBER_DIRECT_STORAGE:\$s32macro_expand_nested_unique_names1SV17directlyAnnotated16NestedMacroInnerfMp_7storagefMu_]]: Int
// CHECK: func $s32macro_expand_nested_unique_names1SV17directlyAnnotated16NestedMacroInnerfMp_8accessorfMu_() -> Int {
// CHECK:   return [[MEMBER_DIRECT_STORAGE]]

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names1SV0C5Outer011NestedMacroF0fMp_.swift
// CHECK: @NestedMacroInner
// CHECK: func $s32macro_expand_nested_unique_names1SV0C5Outer011NestedMacroF0fMp_11nestedOuterfMu_()

// CHECK-LABEL: @__swiftmacro_32macro_expand_nested_unique_names1SV010$s32macro_{{.*}}0kL5InnerfMp_.swift
// CHECK: var [[MEMBER_STORAGE:\$s32macro_expand_nested_unique_names1SV010\$s32macro_.*7storagefMu_]]: Int
// CHECK: func $s32macro_expand_nested_unique_names1SV010$s32macro_{{.*}}8accessorfMu_() -> Int {
// CHECK:   return [[MEMBER_STORAGE]]

// The peers of the directly-annotated function, as a baseline.
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names{{.*}}17directlyAnnotated16NestedMacroInnerfMp_7storagefMu_Sivg
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names{{.*}}17directlyAnnotated16NestedMacroInnerfMp_8accessorfMu_SiyF

// The function introduced by the outer macro...
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names{{.*}}13topLevelOuter011NestedMacroH0fMp_13topH9OuterfMu_yyF

// ...and the peers introduced by the inner macro attached to it.
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names{{.*}}13topLevelOuter{{.*}}Inner{{.*}}7storagefU1_Sivg
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names{{.*}}13topLevelOuter{{.*}}Inner{{.*}}8accessorfU1_SiyF

// The same, in a type context.
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names1SV{{.*}}0C5Outer011NestedMacroF0fMp_11C9OuterfMu_yyF
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names1SV{{.*}}Outer{{.*}}Inner{{.*}}7storagefU1_Sivg
// CHECK-SIL-DAG: sil hidden [ossa] @$s32macro_expand_nested_unique_names1SV{{.*}}Outer{{.*}}Inner{{.*}}8accessorfU1_SiyF
