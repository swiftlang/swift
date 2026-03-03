// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift | %FileCheck --check-prefix SIL %s
// RUN: %target-swift-frontend -emit-ir -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift | %FileCheck --check-prefix IR %s

// RUN: %target-swift-ide-test -print-ast-typechecked -print-access -source-filename=%t/main.swift -load-plugin-library %t/%target-library-name(MacroDefinition) | %FileCheck %s --check-prefix PRINT

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

struct StorageMacro: DeclarationMacro {
  static func expansion(
    of node: some SwiftSyntax.FreestandingMacroExpansionSyntax,
    in context: some SwiftSyntaxMacros.MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["var _property: T"]
  }
}

struct StorageTrampolineMacro: PeerMacro {
  static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    [#"#_Property()"#]
  }
}

//--- main.swift

@freestanding(declaration, names: arbitrary)
macro _Property() = #externalMacro(module: "MacroDefinition", type: "StorageMacro")

@attached(peer, names: prefixed(_))
macro PropertyWrap() = #externalMacro(module: "MacroDefinition", type: "StorageTrampolineMacro")

// Make sure we can lower this without crashing.
public struct S1<T> {
  @PropertyWrap var property: T {
    fatalError()
  }
  public init(_ x: T) {
    self._property = x
  }
}

public struct S2 {
  public typealias T = Bool
  @PropertyWrap var property: Int = 0
  public init(_ x: T) {
    self._property = x
  }
}
// IR: %T4main2S2V = type <{ %TSi, %TSb }>

public class C<T> {
  @PropertyWrap var property: T {
    fatalError()
  }
  public init(_ x: T) {
    self._property = x
  }
}
// SIL:      sil_vtable C {
// SIL-NEXT:   #C._property!getter: <T> (C<T>) -> () -> T : @$s4main1CC9_propertyxvg
// SIL-NEXT:   #C._property!setter: <T> (C<T>) -> (T) -> () : @$s4main1CC9_propertyxvs
// SIL-NEXT:   #C._property!modify: <T> (C<T>) -> () -> () : @$s4main1CC9_propertyxvM

// PRINT-LABEL: struct TestMemberwise1<T>
struct TestMemberwise1<T> {
  @PropertyWrap var property: T
  // PRINT: internal init(property: T, _property: T)

  func foo() {
    // Force the memberwise initializer to be emitted.
    _ = Self.init(property:_property:)
  }
}

// PRINT-LABEL: struct TestMemberwise2<T>
struct TestMemberwise2<T> {
  @PropertyWrap var property: T { fatalError() }
  // PRINT: internal init(_property: T)

  func foo() {
    // Force the memberwise initializer to be emitted.
    _ = Self.init(_property:)
  }
}
