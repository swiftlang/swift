// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -swift-version 5 -emit-module -verify -o %t/Library.swiftmodule -module-name Library %s -load-plugin-library %t/%target-library-name(MacroDefinition) -enable-library-evolution -emit-module-interface-path %t/Library.swiftinterface -emit-tbd -emit-tbd-path %t/Library.tbd -tbd-install_name libLibrary.dylib

// Make sure we can serialize a primary file, where the auxiliary extensions
// are cross-references into their macro expansion buffers.
// RUN: %target-swift-frontend -swift-version 5 -emit-module -verify -o %t/LibraryPrimary.swiftmodule -module-name Library -primary-file %s -load-plugin-library %t/%target-library-name(MacroDefinition)

// Make sure we don't end up with any duplicate decls in the interface or tbd.
// RUN: %FileCheck %s --check-prefix NESTED < %t/Library.swiftinterface
// RUN: %FileCheck %s --check-prefix TOP-LEVEL-PEER < %t/Library.swiftinterface
// RUN: %FileCheck %s --check-prefix MEMBER-PEER < %t/Library.swiftinterface

// Make sure we correctly include a peer of an extension macro.
// RUN: %FileCheck %s --check-prefix EXT-PEER < %t/Library.swiftinterface

// Verify the interface compiles.
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/LibraryFromInterface.swiftmodule %t/Library.swiftinterface -module-name Library
// RUN: %FileCheck %s --check-prefix NESTED-TBD < %t/Library.tbd
// RUN: %FileCheck %s --check-prefix TOP-LEVEL-PEER-TBD < %t/Library.tbd
// RUN: %FileCheck %s --check-prefix MEMBER-PEER-TBD < %t/Library.tbd

public protocol MyProtocol {}

@attached(extension, conformances: MyProtocol)
macro AddMyProtocol() = #externalMacro(module: "MacroDefinition", type: "ConformanceViaExtensionMacro")

@attached(extension, names: named(Nested))
macro AddNested() = #externalMacro(module: "MacroDefinition", type: "NestedConformingExtensionMacro")

@attached(peer, names: named(ConformingPeer))
macro AddConformingPeer() = #externalMacro(module: "MacroDefinition", type: "ConformingPeerStructMacro")

@attached(peer, names: named(MacroPeer))
macro AddPeerStruct() = #externalMacro(module: "MacroDefinition", type: "PeerStructMacro")

// Expands to `@AddPeerStruct extension PeeredOuter { public func extMember() {} }`.
@attached(extension, names: arbitrary)
macro AddPeeredExtension() = #externalMacro(module: "MacroDefinition", type: "PeeredExtensionMacro")

@AddNested
public struct Outer {
  public init() {}
}

// NESTED-COUNT-1: extension Library::Outer.Library::Nested : Library::MyProtocol
// NESTED-NOT: extension Library::Outer.Library::Nested : Library::MyProtocol
// NESTED-TBD-COUNT-1: $s7Library5OuterV6NestedVAA10MyProtocolAAMc
// NESTED-TBD-NOT: $s7Library5OuterV6NestedVAA10MyProtocolAAMc

@AddConformingPeer
public func foo() {}

// TOP-LEVEL-PEER-COUNT-1: extension Library::ConformingPeer : Library::MyProtocol
// TOP-LEVEL-PEER-NOT: extension Library::ConformingPeer : Library::MyProtocol
// TOP-LEVEL-PEER-TBD-COUNT-1: $s7Library14ConformingPeerVAA10MyProtocolAAMc
// TOP-LEVEL-PEER-TBD-NOT: $s7Library14ConformingPeerVAA10MyProtocolAAMc

public struct Outer2 {
  @AddConformingPeer
  public func bar() {}
}

// MEMBER-PEER-COUNT-1: extension Library::Outer2.Library::ConformingPeer : Library::MyProtocol
// MEMBER-PEER-NOT: extension Library::Outer2.Library::ConformingPeer : Library::MyProtocol
// MEMBER-PEER-TBD-COUNT-1: $s7Library6Outer2V14ConformingPeerVAA10MyProtocolAAMc
// MEMBER-PEER-TBD-NOT: $s7Library6Outer2V14ConformingPeerVAA10MyProtocolAAMc

@AddPeeredExtension
public struct PeeredOuter {
  public init() {}
}

// EXT-PEER-DAG: extension Library::PeeredOuter
// EXT-PEER-DAG: public struct MacroPeer
