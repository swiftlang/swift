// REQUIRES: swift_swift_parser
// REQUIRES: OS=macosx
// REQUIRES: CODEGENERATOR=X86

// RUN: %empty-directory(%t)
// RUN: split-file %s %t/src
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/src/macro.swift -g -no-toolchain-stdlib-rpath

// Similar test to `IRGen/macro_expand_extension.swift`, but triggers the exact
// LLVM issue when emitting x86.

// RUN: %target-swift-frontend -c -verify -target x86_64-apple-macos13.0 -module-name main -parse-as-library %t/src/a.swift -o %t/a.o %t/src/b.swift -o %t/b.o -num-threads 2 -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %llvm-nm --defined-only %t/a.o | %FileCheck %s --check-prefix CHECK-A --implicit-check-not Subject
// RUN: %llvm-nm --defined-only %t/b.o | %FileCheck %s --check-prefix CHECK-B

//--- macro.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct NestedEnumExtensionMacro: ExtensionMacro {
  public static func expansion(
    of node: AttributeSyntax,
    attachedTo declaration: some DeclGroupSyntax,
    providingExtensionsOf type: some TypeSyntaxProtocol,
    conformingTo protocols: [TypeSyntax],
    in context: some MacroExpansionContext
  ) throws -> [ExtensionDeclSyntax] {
    let ext: DeclSyntax =
      """
      extension \(type.trimmed): P {
          public enum Kind: Swift.Hashable {
              case a
              case b
          }
          public var kind: Kind { .a }
      }
      """
    return [ext.cast(ExtensionDeclSyntax.self)]
  }
}

//--- a.swift
protocol P {}

@attached(extension, conformances: P, names: arbitrary)
macro AddNestedEnum() = #externalMacro(module: "MacroDefinition", type: "NestedEnumExtensionMacro")

// Make sure a.o is non-empty.
public func dummy() {}
// CHECK-A: $s4main5dummyyyF

//--- b.swift
// A separate file from the macro declaration, so the extended type does not
// belong to the first IRGenModule.
@AddNestedEnum
enum E: Sendable {
  case a(Int)
  case b(String)
}

// Make sure we have both the conformance and metadata.
// CHECK-B-DAG: $s4main1EOAA1PAAMc
// CHECK-B-DAG: $s4main1EO4KindOMn
