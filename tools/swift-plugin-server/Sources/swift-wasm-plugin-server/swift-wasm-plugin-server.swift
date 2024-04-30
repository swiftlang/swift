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

@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling
import Foundation

@main
final class SwiftPluginServer {
  let connection: StandardIOMessageConnection
  var loadedWasmPlugins: [String: WasmPlugin] = [:]
  var hostCapability: PluginMessage.HostCapability?

  init(connection: StandardIOMessageConnection) {
    self.connection = connection
  }

  private func sendMessage(_ message: PluginToHostMessage) throws {
    try connection.sendMessage(message)
  }

  private func loadPluginLibrary(path: String, moduleName: String) async throws -> WasmPlugin {
    // it's worth the effort jumping through these hoops because
    // wasm modules can be really large (30M+) so we want to avoid making
    // copies if possible.
    let data = try NSData(contentsOf: URL(fileURLWithPath: path), options: .mappedIfSafe)
    let wasm = UnsafeByteBuffer(
      data: UnsafeRawBufferPointer(start: data.bytes, count: data.count),
      deallocator: { [box = data as AnyObject] in _ = box }
    )
    return try await defaultWasmPlugin.init(wasm: wasm)
  }

  private func expandMacro(
    moduleName module: String,
    message: HostToPluginMessage
  ) async throws {
    let response: PluginToHostMessage
    do {
      guard let plugin = loadedWasmPlugins[module] else { throw PluginServerError(message: "Could not find module \(module)") }
      let request = try JSON.encode(message).withUnsafeBufferPointer { Data($0) }
      let responseRaw = try await plugin.handleMessage(request)
      response = try responseRaw.withUnsafeBytes {
        try $0.withMemoryRebound(to: UInt8.self) {
          try JSON.decode(PluginToHostMessage.self, from: $0)
        }
      }
    } catch {
      try? FileHandle.standardError.write(contentsOf: Data("Error: \(error)\n".utf8))
      try sendMessage(.expandMacroResult(expandedSource: nil, diagnostics: []))
      return
    }
    try sendMessage(response)
  }

  func run() async throws {
    while let message = try connection.waitForNextMessage(HostToPluginMessage.self) {
      switch message {
      case .getCapability(let hostCapability):
        // Remember the peer capability if provided.
        if let hostCapability = hostCapability {
          self.hostCapability = .init(hostCapability)
        }

        // Return the plugin capability.
        let capability = PluginMessage.PluginCapability(
          protocolVersion: PluginMessage.PROTOCOL_VERSION_NUMBER,
          features: [PluginFeature.loadPluginLibrary.rawValue]
        )
        try sendMessage(.getCapabilityResult(capability: capability))
      case .loadPluginLibrary(let libraryPath, let moduleName):
        do {
          loadedWasmPlugins[moduleName] = try await loadPluginLibrary(path: libraryPath, moduleName: moduleName)
        } catch {
          try? FileHandle.standardError.write(contentsOf: Data("Error: \(error)\n".utf8))
          try sendMessage(.loadPluginLibraryResult(loaded: false, diagnostics: []))
          continue
        }
        try sendMessage(.loadPluginLibraryResult(loaded: true, diagnostics: []))
      case .expandAttachedMacro(let macro, _, _, _, _, _, _, _, _),
           .expandFreestandingMacro(let macro, _, _, _, _):
        try await expandMacro(moduleName: macro.moduleName, message: message)
      #if !SWIFT_PACKAGE
      @unknown default:
        break
      #endif
      }
    }
  }

  /// @main entry point.
  static func main() async throws {
    let connection = try StandardIOMessageConnection()
    try await Self(connection: connection).run()
  }
}

protocol WasmPlugin {
  init(wasm: UnsafeByteBuffer) async throws

  func handleMessage(_ json: Data) async throws -> Data
}

private var defaultWasmPlugin: (some WasmPlugin).Type { DefaultWasmPlugin.self }

// An immutable data buffer with a stable address.
//
// The pointer is valid until the receiver is deallocated.
final class UnsafeByteBuffer: @unchecked Sendable {
  let data: UnsafeRawBufferPointer
  private let deallocator: @Sendable () -> Void

  init(data: UnsafeRawBufferPointer, deallocator: @escaping @Sendable () -> Void) {
    self.data = data
    self.deallocator = deallocator
  }

  deinit { deallocator() }
}

struct PluginServerError: Error, CustomStringConvertible {
  var description: String
  init(message: String) {
    self.description = message
  }
}
