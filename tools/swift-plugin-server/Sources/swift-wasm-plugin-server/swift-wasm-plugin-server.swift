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

import SwiftCompilerPluginMessageHandling
import swiftLLVMJSON
import Foundation
import CSwiftPluginServer

@available(macOS 10.15, *)
@main
final class SwiftPluginServer {
  let connection: PluginHostConnection
  var loadedWasmPlugins: [String: WasmPlugin] = [:]
  var hostCapability: PluginMessage.HostCapability?

  init(connection: PluginHostConnection) {
    self.connection = connection
  }

  private func sendMessage(_ message: PluginToHostMessage) throws {
    try connection.sendMessage(message)
  }

  @MainActor private func loadPluginLibrary(path: String, moduleName: String) async throws -> WasmPlugin {
    guard path.hasSuffix(".wasm") else { throw PluginServerError(message: "swift-wasm-plugin-server can only load wasm") }
    let wasm = try Data(contentsOf: URL(fileURLWithPath: path))
    return try await JSCWasmPlugin(wasm: wasm)
  }

  private func expandMacro(
    moduleName module: String,
    message: HostToPluginMessage
  ) async throws {
    let response: PluginToHostMessage
    do {
      guard let plugin = loadedWasmPlugins[module] else { throw PluginServerError(message: "Could not find module \(module)") }
      let request = try LLVMJSON.encoding(message) { Data(UnsafeRawBufferPointer($0)) }
      let responseRaw = try await plugin.handleMessage(request)
      response = try responseRaw.withUnsafeBytes {
        try $0.withMemoryRebound(to: Int8.self) {
          try LLVMJSON.decode(PluginToHostMessage.self, from: $0)
        }
      }
    } catch {
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
    let connection = try PluginHostConnection()
    try await Self(connection: connection).run()
  }
}

protocol WasmPlugin {
  func handleMessage(_ json: Data) async throws -> Data
}

final class PluginHostConnection {
  let handle: UnsafeRawPointer
  init() throws {
    var errorMessage: UnsafePointer<CChar>? = nil
    guard let handle = PluginServer_createConnection(&errorMessage) else {
      throw PluginServerError(message: String(cString: errorMessage!))
    }
    self.handle = handle
  }

  deinit {
    PluginServer_destroyConnection(self.handle)
  }

  func sendMessage<TX: Encodable>(_ message: TX) throws {
    try LLVMJSON.encoding(message) { buffer in
      try self.sendMessageData(buffer)
    }
  }

  func waitForNextMessage<RX: Decodable>(_ type: RX.Type) throws -> RX? {
    try self.withReadingMessageData { buffer in
      try LLVMJSON.decode(RX.self, from: buffer)
    }
  }

  /// Send a serialized message to the message channel.
  func sendMessageData(_ data: UnsafeBufferPointer<Int8>) throws {
    // Write the header (a 64-bit length field in little endian byte order).
    var header: UInt64 = UInt64(data.count).littleEndian
    let writtenSize = try Swift.withUnsafeBytes(of: &header) { buffer in
      try self.write(buffer: UnsafeRawBufferPointer(buffer))
    }
    guard writtenSize == MemoryLayout.size(ofValue: header) else {
      throw PluginServerError(message: "failed to write message header")
    }

    // Write the body.
    guard try self.write(buffer: UnsafeRawBufferPointer(data)) == data.count else {
      throw PluginServerError(message: "failed to write message body")
    }
  }

  /// Read a serialized message from the message channel and call the 'body'
  /// with the data.
  private func withReadingMessageData<R>(_ body: (UnsafeBufferPointer<Int8>) throws -> R) throws -> R? {
    // Read the header (a 64-bit length field in little endian byte order).
    var header: UInt64 = 0
    let readSize = try Swift.withUnsafeMutableBytes(of: &header) { buffer in
      try self.read(into: UnsafeMutableRawBufferPointer(buffer))
    }
    guard readSize == MemoryLayout.size(ofValue: header) else {
      if readSize == 0 {
        // The host closed the pipe.
        return nil
      }
      // Otherwise, some error happened.
      throw PluginServerError(message: "failed to read message header")
    }

    // Read the body.
    let count = Int(UInt64(littleEndian: header))
    let data = UnsafeMutableBufferPointer<Int8>.allocate(capacity: count)
    defer { data.deallocate() }
    guard try self.read(into: UnsafeMutableRawBufferPointer(data)) == count else {
      throw PluginServerError(message: "failed to read message body")
    }

    // Invoke the handler.
    return try body(UnsafeBufferPointer(data))
  }

  /// Write the 'buffer' to the message channel.
  /// Returns the number of bytes succeeded to write.
  private func write(buffer: UnsafeRawBufferPointer) throws -> Int {
    var bytesToWrite = buffer.count
    guard bytesToWrite > 0 else {
      return 0
    }
    var ptr = buffer.baseAddress!

    while (bytesToWrite > 0) {
      let writtenSize = PluginServer_write(handle, ptr, bytesToWrite)
      if (writtenSize <= 0) {
        // error e.g. broken pipe.
        break
      }
      ptr = ptr.advanced(by: writtenSize)
      bytesToWrite -= Int(writtenSize)
    }
    return buffer.count - bytesToWrite
  }

  /// Read data from the message channel into the 'buffer' up to 'buffer.count' bytes.
  /// Returns the number of bytes succeeded to read.
  private func read(into buffer: UnsafeMutableRawBufferPointer) throws -> Int {
    var bytesToRead = buffer.count
    guard bytesToRead > 0 else {
      return 0
    }
    var ptr = buffer.baseAddress!

    while bytesToRead > 0 {
      let readSize = PluginServer_read(handle, ptr, bytesToRead)
      if (readSize <= 0) {
        // 0: EOF (the host closed), -1: Broken pipe (the host crashed?)
        break;
      }
      ptr = ptr.advanced(by: readSize)
      bytesToRead -= readSize
    }
    return buffer.count - bytesToRead
  }
}

struct PluginServerError: Error, CustomStringConvertible {
  var description: String
  init(message: String) {
    self.description = message
  }
}
