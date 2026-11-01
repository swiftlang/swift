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
import WASI
import WasmTypes

typealias WasmFunction = () throws -> Void

protocol WasmEngine {
  init(pluginPath: FilePath) throws

  func function(named name: String) throws -> WasmFunction?

  func writeToPlugin(_ storage: some Sequence<UInt8>) throws
  func readFromPlugin(into storage: UnsafeMutableRawBufferPointer) throws -> Int

  func shutDown() throws
}

typealias DefaultWasmPlugin = WasmEnginePlugin<DefaultWasmEngine>

// a WasmPlugin implementation that delegates to a WasmEngine
struct WasmEnginePlugin<Engine: WasmEngine>: WasmPlugin {
  private let pumpFunction: WasmFunction
  let engine: Engine

  init(path: FilePath) throws {
    self.engine = try Engine(pluginPath: path)

    let exportName = "swift_wasm_macro_v1_pump"
    guard let pump = try engine.function(named: exportName) else {
      throw WasmEngineError(message: "Wasm plugin has an unknown ABI (could not find '\(exportName)')")
    }
    self.pumpFunction = pump

    guard let start = try engine.function(named: "_start") else {
      throw WasmEngineError(message: "Wasm plugin does not have a '_start' entrypoint")
    }
    try start()
  }

  func handleMessage(_ json: [UInt8]) throws -> [UInt8] {
    try withUnsafeBytes(of: UInt64(json.count).littleEndian) {
      _ = try engine.writeToPlugin($0)
    }
    try engine.writeToPlugin(json)

    try self.pumpFunction()

    let lengthRaw = try withUnsafeTemporaryAllocation(of: UInt8.self, capacity: 8) { buffer in
      let lengthCount = try engine.readFromPlugin(into: UnsafeMutableRawBufferPointer(buffer))
      guard lengthCount == 8 else {
        throw WasmEngineError(message: "Wasm plugin sent invalid response")
      }
      return buffer.withMemoryRebound(to: UInt64.self, \.baseAddress!.pointee)
    }
    let length = Int(UInt64(littleEndian: lengthRaw))
    return try [UInt8](unsafeUninitializedCapacity: length) { buffer, size in
      let received = try engine.readFromPlugin(into: UnsafeMutableRawBufferPointer(buffer))
      guard received == length else {
        throw WasmEngineError(message: "Wasm plugin sent truncated response")
      }
      size = received
    }
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

// `pipe` support on Windows is not included in the latest swift-system release 1.3.1
// but it is available in the latest main branch. Remove the following code when we
// update to the next release of swift-system.
#if os(Windows)
  import ucrt

  var system_errno: CInt {
    var value: CInt = 0
    _ = ucrt._get_errno(&value)
    return value
  }

  extension FileDescriptor {
    static func pipe() throws -> (readEnd: FileDescriptor, writeEnd: FileDescriptor) {
      var fds: (Int32, Int32) = (-1, -1)
      return try withUnsafeMutablePointer(to: &fds) { pointer in
        try pointer.withMemoryRebound(to: Int32.self, capacity: 2) { fds in
          guard _pipe(fds, 4096, _O_BINARY | _O_NOINHERIT) == 0 else {
            throw Errno(rawValue: system_errno)
          }
          return (FileDescriptor(rawValue: fds[0]), FileDescriptor(rawValue: fds[1]))
        }
      }
    }
  }
#endif
