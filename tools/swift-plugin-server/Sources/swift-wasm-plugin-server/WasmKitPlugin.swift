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

#if !os(macOS)

import Foundation
import WasmKit
import WASI

typealias DefaultWasmPlugin = WasmKitPlugin

struct WasmKitPlugin: WasmPlugin {
    let instance: ModuleInstance
    let runtime: Runtime
    let memory: GuestMemory

    init(wasm: Data) throws {
        let module = try parseWasm(bytes: Array(wasm))
        // TODO: we should air-gap this bridge. Wasm macros don't need IO.
        let bridge = try WASIBridgeToHost()
        runtime = Runtime(hostModules: bridge.hostModules)
        instance = try runtime.instantiate(module: module)
        _ = try bridge.start(instance, runtime: runtime)

        let exports = instance.exports
        guard case let .memory(memoryAddr) = exports["memory"] else {
            throw WasmKitPluginError(message: "Wasm plugin does not export a valid memory.")
        }
        self.memory = GuestMemory(store: runtime.store, address: memoryAddr)
    }

    func handleMessage(_ json: Data) throws -> Data {
        let jsonLen = UInt32(json.count)
        let inAddr = try runtime.invoke(instance, function: "wacro_malloc", with: [.i32(jsonLen)])[0].i32
        let rawInAddr = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: inAddr)
        _ = UnsafeGuestBufferPointer(baseAddress: rawInAddr, count: jsonLen)
            .withHostPointer { $0.initialize(from: json) }

        let outAddr = try runtime.invoke(instance, function: "wacro_parse", with: [.i32(inAddr), .i32(jsonLen)])[0].i32
        let outLen = UnsafeGuestPointer<UInt32>(memorySpace: memory, offset: outAddr).pointee
        let outBase = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: outAddr + 4)
        let out = UnsafeGuestBufferPointer(baseAddress: outBase, count: outLen)
            .withHostPointer { Data($0) }

        _ = try runtime.invoke(instance, function: "wacro_free", with: [.i32(outAddr)])

        return out
    }
}

struct WasmKitPluginError: Error, CustomStringConvertible {
    let description: String
    init(message: String) {
        self.description = message
    }
}

#endif
