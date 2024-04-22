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

  init(wasm: Data, imports: WASIBridgeToHost) async throws

  func invoke(_ method: String, _ args: [UInt32]) throws -> [UInt32]
}

typealias DefaultWasmPlugin = WasmEnginePlugin<DefaultWasmEngine>

// a WasmPlugin implementation that delegates to a WasmEngine
struct WasmEnginePlugin<Engine: WasmEngine>: WasmPlugin {
  let engine: Engine

  init(wasm: Data) async throws {
    // TODO: we should air-gap this bridge. Wasm macros don't need IO.
    let bridge = try WASIBridgeToHost()
    engine = try await Engine(wasm: wasm, imports: bridge)
    _ = try engine.invoke("_start", [])
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
