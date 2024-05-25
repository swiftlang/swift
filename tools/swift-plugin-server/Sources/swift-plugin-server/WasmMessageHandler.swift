import SystemPackage
import Dispatch
@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling

/// A `PluginMessageHandler` that intercepts messages intended for Wasm plugins.
final class WasmInterceptingMessageHandler<Base: PluginMessageHandler>: PluginMessageHandler {
  private var loadedWasmPlugins: [String: WasmPlugin] = [:]

  let base: Base
  init(base: Base) {
    self.base = base
  }

  func handleMessage(_ message: HostToPluginMessage) -> PluginToHostMessage {
    switch message {
    case .loadPluginLibrary(let libraryPath, let moduleName):
      guard libraryPath.hasSuffix(".wasm") else { break }
      do {
        loadedWasmPlugins[moduleName] = try defaultWasmPlugin.init(path: FilePath(libraryPath))
      } catch {
        printError("Error: \(error)")
        return .loadPluginLibraryResult(loaded: false, diagnostics: [])
      }
      return .loadPluginLibraryResult(loaded: true, diagnostics: [])
    case .expandAttachedMacro(let macro, _, _, _, _, _, _, _, _),
        .expandFreestandingMacro(let macro, _, _, _, _):
      guard let wasmPlugin = loadedWasmPlugins[macro.moduleName] else { break }
      return expandMacro(plugin: wasmPlugin, message: message)
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
    plugin: WasmPlugin,
    message: HostToPluginMessage
  ) -> PluginToHostMessage {
    do {
      let request = try JSON.encode(message)
      let responseRaw = try plugin.handleMessage(request)
      return try responseRaw.withUnsafeBytes {
        try $0.withMemoryRebound(to: UInt8.self) {
          try JSON.decode(PluginToHostMessage.self, from: $0)
        }
      }
    } catch {
      printError("Error: \(error)")
      return .expandMacroResult(expandedSource: nil, diagnostics: [])
    }
  }
}

protocol WasmPlugin {
  init(path: FilePath) throws

  func handleMessage(_ json: [UInt8]) throws -> [UInt8]
}

private var defaultWasmPlugin: (some WasmPlugin).Type { DefaultWasmPlugin.self }

// TODO: return actual diagnostics instead of using this
private func printError(_ error: String) {
  _ = try? FileDescriptor.standardError.writeAll("\(error)\n".utf8)
}
