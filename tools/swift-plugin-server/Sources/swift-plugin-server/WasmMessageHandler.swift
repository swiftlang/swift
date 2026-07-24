//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SystemPackage
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// A `PluginMessageHandler` that intercepts messages intended for Wasm plugins.
final class WasmInterceptingMessageHandler<Base: PluginMessageHandler>: PluginMessageHandler {
  /// A loaded Wasm guest plus the protocol version it reported via `getCapability`.
  struct LoadedWasmPlugin {
    let plugin: any WasmPlugin
    let protocolVersion: Int
  }

  /// Builds a `WasmPlugin` from a path. Overridable in tests; defaults to the real engine.
  typealias WasmPluginFactory = (FilePath) throws -> any WasmPlugin

  private var loadedWasmPlugins: [String: LoadedWasmPlugin] = [:]

  /// Host capability captured from the incoming `getCapability`, forwarded to each guest on load.
  private var capturedHostCapability: PluginMessage.HostCapability?

  let base: Base
  private let makePlugin: WasmPluginFactory

  init(base: Base, makePlugin: @escaping WasmPluginFactory = { try DefaultWasmPlugin(path: $0) }) {
    self.base = base
    self.makePlugin = makePlugin
  }

  /// Handle the message ourselves if it references a Wasm plugin.
  /// Otherwise, forward it to `base`.
  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    switch message {
    case let .loadPluginLibrary(libraryPath, moduleName):
      guard libraryPath.hasSuffix(".wasm") else { break }
      let libraryFilePath = FilePath(libraryPath)
      var builtPlugin: (any WasmPlugin)?
      do {
        let plugin = try self.makePlugin(libraryFilePath)
        builtPlugin = plugin
        // Forward the host capability so the guest negotiates against the real host
        // version (not the protocol floor), and learn the guest's own protocol version.
        let guestVersion = try self.forwardCapability(to: plugin)
        self.loadedWasmPlugins[moduleName] = LoadedWasmPlugin(plugin: plugin, protocolVersion: guestVersion)
      } catch {
        // A plugin built but failing the handshake never entered loadedWasmPlugins, so
        // shutDown() can't reclaim its pipes. Release them here and surface any error.
        var diagnostics = [PluginMessage.Diagnostic(errorMessage: "\(error)")]
        if let builtPlugin {
          do {
            try builtPlugin.shutDown()
          } catch let shutdownError {
            diagnostics.append(PluginMessage.Diagnostic(
              errorMessage: "additionally, shutting down the failed Wasm plugin errored: \(shutdownError)"))
          }
        }
        return .loadPluginLibraryResult(loaded: false, diagnostics: diagnostics)
      }
      return .loadPluginLibraryResult(loaded: true, diagnostics: [])
    case let .expandAttachedMacro(macro, _, _, attributeSyntax, _, _, _, _, _, _):
      if let response = self.expandMacro(macro, message: message, location: attributeSyntax.location) {
        return response
      } // else break
    case let .expandFreestandingMacro(macro, _, _, syntax, _, _):
      if let response = self.expandMacro(macro, message: message, location: syntax.location) {
        return response
      } // else break
    case let .getCapability(capability):
      // Remember the host's capability so we can forward it to guests on load,
      // then let `base` produce the server's own answer to the compiler.
      self.capturedHostCapability = capability
    #if !SWIFT_PACKAGE
      @unknown default:
        break
    #endif
    }
    return self.base.handleMessage(message)
  }

  func shutDown() throws {
    for loaded in self.loadedWasmPlugins.values {
      try loaded.plugin.shutDown()
    }

    self.loadedWasmPlugins = [:]
  }

  /// Forwards the captured host capability to a freshly loaded guest and returns the
  /// guest's reported protocol version. Falls back to the server's version when the host
  /// sent none, so the guest negotiates against a real version rather than the floor.
  private func forwardCapability(to plugin: any WasmPlugin) throws -> Int {
    let hostCapability = self.capturedHostCapability
      ?? PluginMessage.HostCapability(protocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER)
    let request = try JSON.encode(HostToPluginMessage.getCapability(capability: hostCapability))
    let responseRaw = try plugin.handleMessage(request)
    let response = try responseRaw.withUnsafeBufferPointer {
      try JSON.decode(PluginToHostMessage.self, from: $0)
    }
    guard case .getCapabilityResult(let capability) = response else {
      throw WasmEngineError(message: "Wasm plugin did not answer getCapability with getCapabilityResult")
    }
    return capability.protocolVersion
  }

  /// Protocol version at which the host began encoding `Syntax.Kind.accessor`
  /// (`PluginHost.swift` gates it on `pluginProtocolVersion >= 8`); older guests cannot
  /// decode `.accessor`. Not `PROTOCOL_VERSION_NUMBER`, which can advance past 8 for
  /// unrelated reasons and would then over-reject protocol-8 guests.
  private static var accessorSyntaxProtocolVersion: Int { 8 }

  private func expandMacro(
    _ macro: PluginMessage.MacroReference,
    message: HostToPluginMessage,
    location: PluginMessage.SourceLocation?
  ) -> PluginToHostMessage? {
    guard let loaded = self.loadedWasmPlugins[macro.moduleName] else { return nil }

    // The host encodes outgoing syntax at the server's protocol version, so an older guest
    // cannot decode `.accessor` (protocol >= 8). Reject it here, checking every syntax field
    // and lexicalContext, rather than forwarding a message the guest cannot parse.
    // Non-accessor traffic to an old guest still expands.
    if loaded.protocolVersion < Self.accessorSyntaxProtocolVersion, Self.messageContainsAccessorKind(message) {
      return .expandMacroResult(
        expandedSource: nil,
        diagnostics: [PluginMessage.Diagnostic(
          errorMessage: """
          cannot expand macro '\(macro.name)' from '\(macro.moduleName).\(macro.typeName)': the Wasm \
          macro plugin reports protocol version \(loaded.protocolVersion), but the host requires \
          protocol version \(Self.accessorSyntaxProtocolVersion) to send 'accessor' syntax; \
          rebuild the plugin against a newer swift-syntax
          """,
          position: location?.position ?? .invalid
        )]
      )
    }

    do {
      let request = try JSON.encode(message)
      let responseRaw = try loaded.plugin.handleMessage(request)
      return try responseRaw.withUnsafeBufferPointer {
        try JSON.decode(PluginToHostMessage.self, from: $0)
      }
    } catch {
      return .expandMacroResult(
        expandedSource: nil,
        diagnostics: [PluginMessage.Diagnostic(
          errorMessage: """
          failed to communicate with external macro implementation type \
          '\(macro.moduleName).\(macro.typeName)' to expand macro '\(macro.name)'; the plugin reports \
          protocol version \(loaded.protocolVersion) and the host uses \
          \(PluginMessage.PROTOCOL_VERSION_NUMBER); \(error)
          """,
          position: location?.position ?? .invalid
        )]
      )
    }
  }

  /// True if any syntax node in the message, including lexicalContext, has kind `.accessor`.
  /// Checks every syntax field, not only the ones that carry `.accessor` today (declSyntax,
  /// parentDeclSyntax, lexicalContext), so the guard still holds if the host version-gates
  /// other fields.
  private static func messageContainsAccessorKind(_ message: HostToPluginMessage) -> Bool {
    switch message {
    case let .expandAttachedMacro(_, _, _, attr, decl, parent, ext, conf, lexical, _):
      return [attr, decl, parent, ext, conf].contains { $0?.kind == .accessor }
        || (lexical ?? []).contains { $0.kind == .accessor }
    case let .expandFreestandingMacro(_, _, _, syntax, lexical, _):
      return syntax.kind == .accessor || (lexical ?? []).contains { $0.kind == .accessor }
    default:
      return false
    }
  }
}

fileprivate extension PluginMessage.Diagnostic {
  init(
    errorMessage: String,
    position: PluginMessage.Diagnostic.Position = .invalid
  ) {
    self.init(
      message: errorMessage,
      severity: .error,
      position: position,
      highlights: [],
      notes: [],
      fixIts: []
    )
  }
}

fileprivate extension PluginMessage.SourceLocation {
  var position: PluginMessage.Diagnostic.Position {
    .init(fileName: fileName, offset: offset)
  }
}

protocol WasmPlugin {
  init(path: FilePath) throws

  func handleMessage(_ json: [UInt8]) throws -> [UInt8]

  func shutDown() throws
}
