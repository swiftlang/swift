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

    init(wasm: Data) throws {
        let module = try parseWasm(bytes: Array(wasm))
        // TODO: we should air-gap this bridge. Wasm macros don't need IO.
        let bridge = try WASIBridgeToHost()
        runtime = Runtime(hostModules: bridge.hostModules)
        instance = try runtime.instantiate(module: module)
        _ = try bridge.start(instance, runtime: runtime)
    }

    func handleMessage(_ json: Data) throws -> Data {
        let exports = instance.exports
        guard case let .memory(memoryAddr) = exports["memory"] else { fatalError("bad memory") }
        guard case let .function(malloc) = exports["wacro_malloc"] else { fatalError("bad wacro_malloc") }
        guard case let .function(parse) = exports["wacro_parse"] else { fatalError("bad wacro_parse") }
        guard case let .function(free) = exports["wacro_free"] else { fatalError("bad wacro_free") }
        let memory = GuestMemory(store: runtime.store, address: memoryAddr)

        let jsonLen = UInt32(json.count)
        let inAddr = try malloc.invoke([.i32(jsonLen)], runtime: runtime)[0].i32
        let rawInAddr = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: inAddr)
        _ = UnsafeGuestBufferPointer(baseAddress: rawInAddr, count: jsonLen)
            .withHostPointer { $0.initialize(from: json) }

        let outAddr = try parse.invoke([.i32(inAddr), .i32(jsonLen)], runtime: runtime)[0].i32
        let outLen = UnsafeGuestPointer<UInt32>(memorySpace: memory, offset: outAddr).pointee
        let outBase = UnsafeGuestPointer<UInt8>(memorySpace: memory, offset: outAddr + 4)
        let out = UnsafeGuestBufferPointer(baseAddress: outBase, count: outLen).withHostPointer { Data($0) }

        _ = try free.invoke([.i32(outAddr)], runtime: runtime)

        return out
    }
}

#endif
