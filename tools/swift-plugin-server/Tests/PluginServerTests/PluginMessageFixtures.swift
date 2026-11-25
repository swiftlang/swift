//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

extension PluginMessage.Syntax {
  static func fixture(kind: Kind, source: String, offset: Int = 0) -> Self {
    PluginMessage.Syntax(
      kind: kind,
      source: source,
      location: PluginMessage.SourceLocation(
        fileID: "MyApp/test.swift", fileName: "test.swift", offset: offset, line: 1, column: offset + 1
      )
    )
  }
}

extension HostToPluginMessage {
  static func freestandingExpansion(
    moduleName: String,
    syntaxKind: PluginMessage.Syntax.Kind = .expression,
    lexicalContext: [PluginMessage.Syntax]? = nil
  ) -> Self {
    .expandFreestandingMacro(
      macro: PluginMessage.MacroReference(moduleName: moduleName, typeName: "ConstMacro", name: "constInt"),
      macroRole: .expression,
      discriminator: "$disc",
      syntax: .fixture(kind: syntaxKind, source: "#constInt"),
      lexicalContext: lexicalContext
    )
  }

  /// An expansion of `@Foo var x: Int`.
  static func attachedExpansion(
    moduleName: String,
    declKind: PluginMessage.Syntax.Kind,
    lexicalContext: [PluginMessage.Syntax]? = nil
  ) -> Self {
    .expandAttachedMacro(
      macro: PluginMessage.MacroReference(moduleName: moduleName, typeName: "ConstMacro", name: "constInt"),
      macroRole: .accessor,
      discriminator: "$disc",
      attributeSyntax: .fixture(kind: .attribute, source: "@Foo"),
      declSyntax: .fixture(kind: declKind, source: "var x: Int", offset: 5),
      parentDeclSyntax: nil,
      extendedTypeSyntax: nil,
      conformanceListSyntax: nil,
      lexicalContext: lexicalContext
    )
  }
}

extension PluginToHostMessage {
  var isSuccessfulLoad: Bool {
    guard case .loadPluginLibraryResult(loaded: true, let diagnostics) = self else { return false }
    return diagnostics.isEmpty
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
