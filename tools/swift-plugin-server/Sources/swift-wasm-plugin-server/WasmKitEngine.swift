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

#if !SWIFT_WASM_USE_JSC

import WASI
import WasmKit
import WasmKitWASI

typealias DefaultWasmEngine = WasmKitEngine

struct WasmKitEngine: WasmEngine {
  private let module: Module
  private let instance: ModuleInstance
  private let runtime: Runtime
  let memory: WasmKitGuestMemory

  init(wasm: UnsafeByteBuffer, imports: WASIBridgeToHost) throws {
    // we never call wasm.deallocator, effectively leaking the data,
    // but that's intentional because plugins can't be "unloaded"
    module = try parseWasm(bytes: Array(wasm.data))
    runtime = Runtime(hostModules: imports.hostModules)
    instance = try runtime.instantiate(module: module)

    let exports = instance.exports
    guard case let .memory(memoryAddr) = exports["memory"] else {
      throw WasmKitPluginError(message: "Wasm plugin does not export a valid memory.")
    }
    self.memory = WasmKitGuestMemory(store: runtime.store, address: memoryAddr)
  }

  func customSections(named name: String) throws -> [ArraySlice<UInt8>] {
    module.customSections.filter { $0.name == name }.map(\.bytes)
  }

  func invoke(_ method: String, _ args: [UInt32]) throws -> [UInt32] {
    try runtime.invoke(instance, function: method, with: args.map(Value.i32)).map(\.i32)
  }
}

struct WasmKitPluginError: Error, CustomStringConvertible {
    let description: String
    init(message: String) {
        self.description = message
    }
}

#endif
