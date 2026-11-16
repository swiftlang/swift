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

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

import SystemPackage

typealias WasmFunction = () throws -> Void

protocol WasmEngine {
  init(pluginPath: FilePath) throws

  func function(named name: String) throws -> WasmFunction?

  func writeToPlugin(_ storage: some Sequence<UInt8>) throws
  /// Copies the plugin's oldest unread output into `storage` and returns how many bytes it
  /// copied, fewer than `storage.count` only when the plugin has written no more.
  func readFromPlugin(into storage: UnsafeMutableRawBufferPointer) throws -> Int

  func shutDown() throws
}

typealias DefaultWasmPlugin = WasmEnginePlugin<DefaultWasmEngine>

// a WasmPlugin implementation that delegates to a WasmEngine
struct WasmEnginePlugin<Engine: WasmEngine>: WasmPlugin {
  private let pumpFunction: WasmFunction
  let engine: Engine

  init(path: FilePath) throws {
    let engine = try Engine(pluginPath: path)
    do {
      self.pumpFunction = try Self.start(engine)
    } catch {
      try Cleanup.throwAfterRunning(engine.shutDown, error: error)
    }
    self.engine = engine
  }

  private static func start(_ engine: Engine) throws -> WasmFunction {
    let exportName = "swift_wasm_macro_v1_pump"
    guard let pump = try engine.function(named: exportName) else {
      throw WasmEngineError(message: "Wasm plugin has an unknown ABI (could not find '\(exportName)')")
    }

    guard let start = try engine.function(named: "_start") else {
      throw WasmEngineError(message: "Wasm plugin does not have a '_start' entrypoint")
    }
    try start()
    return pump
  }

  func handleMessage(_ json: [UInt8]) throws -> [UInt8] {
    try withUnsafeBytes(of: UInt64(json.count).littleEndian) {
      try engine.writeToPlugin($0)
    }
    try engine.writeToPlugin(json)

    try self.pumpFunction()

    var lengthRaw: UInt64 = 0
    let lengthCount = try withUnsafeMutableBytes(of: &lengthRaw) { try engine.readFromPlugin(into: $0) }
    guard lengthCount == 8 else {
      throw WasmEngineError(message: "Wasm plugin sent invalid response")
    }
    let length = UInt64(littleEndian: lengthRaw)
    // Do not allocate the guest-supplied length before the bytes arrive.
    var response: [UInt8] = []
    try withUnsafeTemporaryAllocation(byteCount: 64 * 1024, alignment: 1) { chunk in
      while UInt64(response.count) < length {
        let wanted = Int(min(length - UInt64(response.count), UInt64(chunk.count)))
        let received = try engine.readFromPlugin(into: UnsafeMutableRawBufferPointer(rebasing: chunk[..<wanted]))
        guard received == wanted else {
          throw WasmEngineError(message: "Wasm plugin sent truncated response")
        }
        response.append(contentsOf: chunk[..<received])
      }
    }
    return response
  }

  func shutDown() throws {
    try self.engine.shutDown()
  }
}

struct WasmEngineError: Error, CustomStringConvertible {
  let description: String

  init(message: String) {
    self.description = message
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
