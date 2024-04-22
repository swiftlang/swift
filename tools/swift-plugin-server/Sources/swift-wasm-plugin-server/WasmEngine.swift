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
import Foundation

protocol WasmEngine {
  associatedtype GuestMemoryType: GuestMemory
  var memory: GuestMemoryType { get }

  init(wasm: UnsafeByteBuffer, imports: WASIBridgeToHost) async throws

  func customSections(named name: String) throws -> [ArraySlice<UInt8>]

  func invoke(_ method: String, _ args: [UInt32]) throws -> [UInt32]
}

typealias DefaultWasmPlugin = WasmEnginePlugin<DefaultWasmEngine>

// a WasmPlugin implementation that delegates to a WasmEngine
struct WasmEnginePlugin<Engine: WasmEngine>: WasmPlugin {
  let engine: Engine

  init(wasm: UnsafeByteBuffer) async throws {
    // TODO: we should air-gap this bridge. Wasm macros don't need IO.
    let bridge = try WASIBridgeToHost()
    engine = try await Engine(wasm: wasm, imports: bridge)
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
    let sectionName = "wacro_abi"
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

  func handleMessage(_ json: Data) async throws -> Data {
    let memory = engine.memory

    let jsonLen = UInt32(json.count)
    let inAddr = try engine.invoke("wacro_malloc", [jsonLen])[0]
    let rawInAddr = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: inAddr)
    _ = UnsafeGuestBufferPointer(baseAddress: rawInAddr, count: jsonLen)
      .withHostPointer { $0.initialize(from: json) }

    let outAddr = try engine.invoke("wacro_parse", [inAddr, jsonLen])[0]
    let outLen = UnsafeGuestPointer<UInt32>(memorySpace: memory, offset: outAddr).pointee
    let outBase = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: outAddr + 4)
    let out = UnsafeGuestBufferPointer(baseAddress: outBase, count: outLen)
      .withHostPointer { Data($0) }

    _ = try engine.invoke("wacro_free", [outAddr])

    return out
  }
}

struct WasmEngineError: Error, CustomStringConvertible {
  let description: String

  init(message: String) {
    self.description = message
  }
}
