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

struct WasmKitMacroRunner: WasmPlugin {
    let instance: ModuleInstance
    let runtime: Runtime

    init(wasm: Data) throws {
        let module = try parseWasm(bytes: Array(wasm))
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

        let inAddr = try malloc.invoke([.i32(UInt32(json.count))], runtime: runtime)[0].i32

        runtime.store.withMemory(at: memoryAddr) { mem in
            mem.data.replaceSubrange(Int(inAddr)..<(Int(inAddr) + json.count), with: json)
        }

        let outAddr = try parse.invoke([.i32(inAddr), .i32(UInt32(json.count))], runtime: runtime)[0].i32
        let out = runtime.store.withMemory(at: memoryAddr) { mem in
            let bytes = Array(mem.data[Int(outAddr)..<(Int(outAddr) + 4)])
            let len =
              (UInt32(bytes[0]) << 0)  |
              (UInt32(bytes[1]) << 8)  |
              (UInt32(bytes[2]) << 16) |
              (UInt32(bytes[3]) << 24)
            return Data(mem.data[(Int(outAddr) + 4)...].prefix(Int(len)))
        }

        _ = try free.invoke([.i32(outAddr)], runtime: runtime)

        return out
    }
}

#endif
