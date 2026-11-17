// rdar://186052220 (https://github.com/swiftlang/swift/issues/91799)
// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct ProperPassthroughMacro: BodyMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingBodyFor declaration: some DeclSyntaxProtocol & WithOptionalCodeBlockSyntax,
    in context: some MacroExpansionContext
  ) throws -> [CodeBlockItemSyntax] {
    guard let body = declaration.body else { return [] }
    return Array(body.statements)
  }
}

//--- main.swift
@attached(body)
macro ProperPassthrough() = #externalMacro(module: "MacroDefinition", type: "ProperPassthroughMacro")

final class Subject {
  var value = 0
  func run(_ body: @escaping (Int) -> Void) {}
}

extension Subject {
  func baseline() {
    self.run { [weak self] _ in self?.value += 1 }
  }

  @ProperPassthrough // expected-note {{in expansion of macro 'ProperPassthrough' on instance method 'weakSelf()' here}}
  func weakSelf() {
    self.run { [weak self] _ in self?.value += 1 }
  }
  // expected-expansion@-3:19 {{
  //   expected-error@2:10 {{failed to produce diagnostic for expression; please submit a bug report}}
  // }}

  @ProperPassthrough // expected-note {{in expansion of macro 'ProperPassthrough' on instance method 'localCapture()' here}}
  func localCapture() {
    let n = 1
    self.run { [n] _ in self.value += n }
  }
  // expected-expansion@-4:23 {{
  //   expected-error@3:10 {{failed to produce diagnostic for expression; please submit a bug report}}
  // }}

  @ProperPassthrough
  func noCaptureList() {
    self.run { _ in self.value += 1 }
  }
}
