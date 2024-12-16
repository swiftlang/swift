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
  private var loadedWasmPlugins: [String: any WasmPlugin] = [:]

  let base: Base
  init(base: Base) {
    self.base = base
  }

  /// Handle the message ourselves if it references a Wasm plugin.
  /// Otherwise, forward it to `base`.
  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    switch message {
    case let .loadPluginLibrary(libraryPath, moduleName):
      guard libraryPath.hasSuffix(".wasm") else { break }
      let libraryFilePath = FilePath(libraryPath)
      do {
        self.loadedWasmPlugins[moduleName] = try DefaultWasmPlugin(path: libraryFilePath)
      } catch {
        return .loadPluginLibraryResult(
          loaded: false,
          diagnostics: [PluginMessage.Diagnostic(errorMessage: "\(error)")]
        )
      }
      return .loadPluginLibraryResult(loaded: true, diagnostics: [])
    case let .expandAttachedMacro(macro, _, _, syntax, _, _, _, _, _),
         let .expandFreestandingMacro(macro, _, _, syntax, _):
      if let response = self.expandMacro(macro, message: message, location: syntax.location) {
        return response
      } // else break
    case .getCapability:
      break
    #if !SWIFT_PACKAGE
      @unknown default:
        break
    #endif
    }
    return self.base.handleMessage(message)
  }

  func shutDown() throws {
    for plugin in self.loadedWasmPlugins.values {
      try plugin.shutDown()
    }

    self.loadedWasmPlugins = [:]
  }

  private func expandMacro(
    _ macro: PluginMessage.MacroReference,
    message: HostToPluginMessage,
    location: PluginMessage.SourceLocation?
  ) -> PluginToHostMessage? {
    guard let plugin = self.loadedWasmPlugins[macro.moduleName] else { return nil }
    do {
      let request = try JSON.encode(message)
      let responseRaw = try plugin.handleMessage(request)
      return try responseRaw.withUnsafeBytes {
        try $0.withMemoryRebound(to: UInt8.self) {
          try JSON.decode(PluginToHostMessage.self, from: $0)
        }
      }
    } catch {
      return .expandMacroResult(
        expandedSource: nil,
        diagnostics: [PluginMessage.Diagnostic(
          errorMessage: """
          failed to communicate with external macro implementation type \
          '\(macro.moduleName).\(macro.typeName)' to expand macro '\(macro.name)'; \
          \(error)
          """,
          position: location?.position ?? .invalid
        )]
      )
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
