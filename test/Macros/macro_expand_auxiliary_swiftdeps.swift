// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Make sure we pick up extension macros and top-level peers in swiftdeps.

// RUN: %target-swift-frontend -typecheck -module-name main -primary-file %s -load-plugin-library %t/%target-library-name(MacroDefinition) -emit-reference-dependencies-path %t/deps.swiftdeps
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/deps.swiftdeps | %FileCheck %s

public protocol MyProtocol {}

@attached(extension, conformances: MyProtocol,
          names: named(macroAddedFunc), named(MacroAddedNested))
macro AddExtensionWithMembers() = #externalMacro(module: "MacroDefinition", type: "ExtensionWithMembersMacro")

@attached(peer, names: named(MacroPeer))
macro AddPeerStruct() = #externalMacro(module: "MacroDefinition", type: "PeerStructMacro")

@AddExtensionWithMembers
public struct Outer {}

// CHECK-DAG: member interface 4main5OuterV macroAddedFunc true
// CHECK-DAG: member interface 4main5OuterV MacroAddedNested true
// CHECK-DAG: nominal interface 4main5OuterV16MacroAddedNestedV '' true
// CHECK-DAG: potentialMember interface 4main5OuterV16MacroAddedNestedV '' true

@AddPeerStruct
public struct Anchor {}

// CHECK-DAG: topLevel interface '' MacroPeer true
// CHECK-DAG: nominal interface 4main9MacroPeerV '' true
// CHECK-DAG: potentialMember interface 4main9MacroPeerV '' true
// CHECK-DAG: nominal interface 4main9MacroPeerV6NestedV '' true
// CHECK-DAG: potentialMember interface 4main9MacroPeerV6NestedV '' true
