import SystemPackage
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// A `PluginMessageHandler` that intercepts messages intended for Wasm plugins.
final class WasmInterceptingMessageHandler<Base: PluginMessageHandler>: PluginMessageHandler {
  private var loadedWasmPlugins: [String: WasmPlugin] = [:]

  let base: Base
  init(base: Base) {
    self.base = base
  }

  /// Handle the message ourselves if it references a Wasm plugin.
  /// Otherwise, forward it to `base`.
  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    switch message {
    case .loadPluginLibrary(let libraryPath, let moduleName):
      guard libraryPath.hasSuffix(".wasm") else { break }
      let libraryFilePath = FilePath(libraryPath)
      do {
        loadedWasmPlugins[moduleName] = try defaultWasmPlugin.init(path: libraryFilePath)
      } catch {
        return .loadPluginLibraryResult(
          loaded: false,
          diagnostics: [PluginMessage.Diagnostic(errorMessage: "\(error)")]
        )
      }
      return .loadPluginLibraryResult(loaded: true, diagnostics: [])
    case .expandAttachedMacro(let macro, _, _, _, _, _, _, _, _),
        .expandFreestandingMacro(let macro, _, _, _, _):
      if let response = expandMacro(macro, message: message) {
        return response
      } // else break
    case .getCapability:
      break
#if !SWIFT_PACKAGE
    @unknown default:
      break
#endif
    }
    return base.handleMessage(message)
  }

  private func expandMacro(
    _ macro: PluginMessage.MacroReference,
    message: HostToPluginMessage
  ) -> PluginToHostMessage? {
    guard let plugin = loadedWasmPlugins[macro.moduleName] else { return nil }
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
          '\(macro.moduleName).\(macro.typeName)' to expand macro '\(macro.name)()'; \
          \(error)
          """
        )]
      )
    }
  }
}

extension PluginMessage.Diagnostic {
  fileprivate init(errorMessage: String) {
    self.init(
      message: errorMessage,
      severity: .error,
      position: .invalid,
      highlights: [],
      notes: [],
      fixIts: []
    )
  }
}

protocol WasmPlugin {
  init(path: FilePath) throws

  func handleMessage(_ json: [UInt8]) throws -> [UInt8]
}

private var defaultWasmPlugin: (some WasmPlugin).Type { DefaultWasmPlugin.self }
