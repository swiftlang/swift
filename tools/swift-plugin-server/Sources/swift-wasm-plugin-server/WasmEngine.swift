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

import WASI
import WasmTypes
import SystemPackage

protocol WasmEngine {
  init(path: FilePath, imports: WASIBridgeToHost) async throws

  func customSections(named name: String) throws -> [ArraySlice<UInt8>]

  func invoke(_ method: String, _ args: [UInt32]) throws -> [UInt32]
}

typealias DefaultWasmPlugin = WasmEnginePlugin<DefaultWasmEngine>

// a WasmPlugin implementation that delegates to a WasmEngine
struct WasmEnginePlugin<Engine: WasmEngine>: WasmPlugin {
  private let hostToPlugin: FileDescriptor
  private let pluginToHost: FileDescriptor
  let engine: Engine

  init(path: FilePath) async throws {
    let hostToPluginPipes = try FileDescriptor.pipe()
    let pluginToHostPipes = try FileDescriptor.pipe()
    self.hostToPlugin = hostToPluginPipes.writeEnd
    self.pluginToHost = pluginToHostPipes.readEnd

    let bridge = try WASIBridgeToHost(
      stdin: hostToPluginPipes.readEnd,
      stdout: pluginToHostPipes.writeEnd,
      stderr: .standardError
    )
    engine = try await Engine(path: path, imports: bridge)
    try checkABIVersion()
    _ = try engine.invoke("_start", [])
  }

  private func checkABIVersion() throws {
    let abiVersion = try abiVersion()
    guard abiVersion == 1 else {
      throw WasmEngineError(message: "Wasm plugin has unsupported ABI version: \(abiVersion)")
    }
  }

  private func abiVersion() throws -> UInt32 {
    let sectionName = "swift_wasm_macro_abi"
    let sections = try engine.customSections(named: sectionName)
    switch sections.count {
    case 0:
      throw WasmEngineError(message: "Wasm macro is missing a '\(sectionName)' section")
    case 1:
      break
    default:
      throw WasmEngineError(message: "Wasm macro has too many '\(sectionName)' sections. Expected one, got \(sections.count)")
    }
    let section = sections[0]
    guard section.count == 4 else {
      throw WasmEngineError(message: """
      Wasm macro has incorrect '\(sectionName)' section length. Expected 4 bytes, got \(section.count).
      """)
    }
    return section.withUnsafeBufferPointer { buffer in
      buffer.withMemoryRebound(to: UInt32.self) {
        UInt32(littleEndian: $0.baseAddress!.pointee)
      }
    }
  }

  func handleMessage(_ json: [UInt8]) async throws -> [UInt8] {
    try withUnsafeBytes(of: UInt64(json.count).littleEndian) {
      _ = try hostToPlugin.writeAll($0)
    }
    try hostToPlugin.writeAll(json)

    _ = try engine.invoke("swift_wasm_macro_pump", [])

    let lengthRaw = try withUnsafeTemporaryAllocation(of: UInt8.self, capacity: 8) { buffer in
      let lengthCount = try pluginToHost.read(into: UnsafeMutableRawBufferPointer(buffer))
      guard lengthCount == 8 else {
        throw WasmEngineError(message: "Wasm plugin sent invalid response")
      }
      return buffer.withMemoryRebound(to: UInt64.self, \.baseAddress!.pointee)
    }
    let length = Int(UInt64(littleEndian: lengthRaw))
    return try [UInt8](unsafeUninitializedCapacity: length) { buffer, size in
      let received = try pluginToHost.read(into: UnsafeMutableRawBufferPointer(buffer))
      guard received == length else {
        throw WasmEngineError(message: "Wasm plugin sent truncated response")
      }
      size = received
    }
  }
}

struct WasmEngineError: Error, CustomStringConvertible {
  let description: String

  init(message: String) {
    self.description = message
  }
}
