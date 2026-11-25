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

@testable import swift_plugin_server
import Foundation
import Synchronization
import SystemPackage
import Testing
@_spi(WASIPlatform) import WASI
import WasmKit
import WAT

private let echo = #"""
(module
  (import "wasi_snapshot_preview1" "fd_read"
    (func $fd_read
      (param (; fd ;) i32 (; ioVecOffset ;) i32 (; ioVecCount ;) i32 (; readCountPointer ;) i32)
      (result (; errno ;) i32)
    )
  )

  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write
      (param (; fd ;) i32 (; ioVecOffset ;) i32 (; ioVecCount ;) i32 (; writtenCountPointer ;) i32)
      (result (; errno ;) i32)
    )
  )

  (export "_start" (func $start))
  (export "swift_wasm_macro_v1_pump" (func $pump))
  (memory $memory 1)
  (export "memory" (memory $memory))

  (func $start)

  (func $pump
    (memory.fill
      (i32.const 0)
      (i32.const 0)
      (i32.const 1024)
    )

    (i32.store
      (i32.const 0) (; first byte is the buffer pointer ;)
      (i32.const 8) (; 4 bytes for the pointer + 4 bytes for the count ;)
    )
    (i32.store
      (i32.const 4   ) (; 4 bytes already used for the pointer ;)
      (i32.const 1024) (; 1024 bytes should be enough for everyone (probably) ;)
    )

    (call $fd_read
      (i32.const 0) (; stdin FD    ;)
      (i32.const 0) (; ioVecOffset ;)
      (i32.const 1) (; ioVecCount  ;)
      (i32.const 1028) (; skip buffer and its count and store read counter after that ;)
    )
    (drop) (; ignore the result ;)

    (call $fd_write
      (i32.const 1) (; stdout FD    ;)
      (i32.const 0) (; ioVecOffset ;)
      (i32.const 1) (; ioVecCount  ;)
      (i32.const 1028) (; skip buffer and its count and store write counter after that ;)
    )
    (drop) (; ignore the result ;)
  )
)
"""#

private let emptyPlugin = #"""
(module
  (memory 1)
  (export "memory" (memory 0))
  (func $start)
  (export "_start" (func $start))
  (func $pump)
  (export "swift_wasm_macro_v1_pump" (func $pump))
)
"""#

private let missingPump = #"""
(module
  (memory 1)
  (export "memory" (memory 0))
  (func $start)
  (export "_start" (func $start))
)
"""#

private let missingStart = #"""
(module
  (memory 1)
  (export "memory" (memory 0))
  (func $pump)
  (export "swift_wasm_macro_v1_pump" (func $pump))
)
"""#

private let unresolvedImport = #"""
(module
  (import "env" "missing" (func))
  (memory 1)
  (export "memory" (memory 0))
)
"""#

/// Reads one request in a single `fd_read` and writes the bytes back as the reply.
private let framedEcho = #"""
(module
  (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 4)
  (export "memory" (memory 0))
  (func $start)
  (export "_start" (func $start))
  (func $pump
    (i32.store (i32.const 0) (i32.const 16))
    (i32.store (i32.const 4) (i32.const 0x3fff0))
    (drop (call $fd_read (i32.const 0) (i32.const 0) (i32.const 1) (i32.const 8)))
    (i32.store (i32.const 4) (i32.load (i32.const 8)))
    (drop (call $fd_write (i32.const 1) (i32.const 0) (i32.const 1) (i32.const 8)))
  )
  (export "swift_wasm_macro_v1_pump" (func $pump))
)
"""#

private func guestWriting(_ reply: [UInt8]) -> String {
  let data = reply.map { String(format: #"\%02x"#, $0) }.joined()
  return #"""
  (module
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
    (memory 1)
    (export "memory" (memory 0))
    (data (i32.const 16) "\#(data)")
    (func $start)
    (export "_start" (func $start))
    (func $pump
      (i32.store (i32.const 0) (i32.const 16))
      (i32.store (i32.const 4) (i32.const \#(reply.count)))
      (drop (call $fd_write (i32.const 1) (i32.const 0) (i32.const 1) (i32.const 8)))
    )
    (export "swift_wasm_macro_v1_pump" (func $pump))
  )
  """#
}

/// Stands in for a pipe transport: the plugin's output is the bytes of the file at
/// `pluginPath`, and a read with none left throws where a pipe would block.
private struct BlockingReadEngine: WasmEngine {
  struct WouldBlock: Error {}

  private let output = WasmStdioBuffer(accessMode: .write)

  init(pluginPath: FilePath) throws {
    output.append(try Data(contentsOf: URL(fileURLWithPath: pluginPath.string)))
  }

  func function(named name: String) throws -> WasmFunction? { {} }

  func writeToPlugin(_ storage: some Sequence<UInt8>) throws {}

  func readFromPlugin(into storage: UnsafeMutableRawBufferPointer) throws -> Int {
    let count = output.take(into: storage)
    guard count > 0 || storage.isEmpty else { throw WouldBlock() }
    return count
  }

  func shutDown() throws {}
}

private final class ResultBox<R: Sendable>: Sendable {
  let result = Mutex<Result<R, any Error>?>(nil)
}

private func runWithTimeout<R: Sendable>(
  _ timeout: DispatchTimeInterval,
  _ body: @escaping @Sendable () throws -> R
) -> Result<R, any Error>? {
  let finished = DispatchSemaphore(value: 0)
  let box = ResultBox<R>()
  DispatchQueue.global().async {
    let result = Result { try body() }
    box.result.withLock { $0 = result }
    finished.signal()
  }
  guard finished.wait(timeout: .now() + timeout) == .success else { return nil }
  return box.result.withLock { $0 }
}

private func exchange(_ request: [UInt8], withPluginAt path: FilePath) throws -> [UInt8] {
  let plugin = try WasmEnginePlugin<WasmKitEngine>(path: path)
  let response = Result { try plugin.handleMessage(request) }
  try plugin.shutDown()
  return try response.get()
}

private func withPluginFile<R>(_ bytes: [UInt8], _ body: (FilePath) throws -> R) throws -> R {
  let url = FileManager.default.temporaryDirectory.appendingPathComponent("\(UUID().uuidString).wasm")
  try Data(bytes).write(to: url)
  defer { try! FileManager.default.removeItem(at: url) }
  return try body(FilePath(url.path))
}

private func openPipeCount() throws -> Int? {
  guard FileManager.default.fileExists(atPath: "/dev/fd") else { return nil }
  return try FileManager.default.contentsOfDirectory(atPath: "/dev/fd").filter { name in
    guard let descriptor = CInt(name) else { return false }
    do {
      return try FileDescriptor(rawValue: descriptor).stat().type == .fifo
    } catch Errno.badFileDescriptor {
      // The listing includes its own descriptor, which is closed by now.
      return false
    }
  }.count
}

@Suite
struct WasmEngineTests {
  @Test
  func basic() throws {
    try withPluginFile(wat2wasm(echo)) { path in
      let wasmKit = try WasmEnginePlugin<WasmKitEngine>(path: path)
      let input: [UInt8] = [1,2,3,4,5]
      #expect(try wasmKit.handleMessage(input) == input)
      try wasmKit.shutDown()
    }
  }

  @Test
  func loadedPluginOpensNoPipes() throws {
    try withPluginFile(wat2wasm(emptyPlugin)) { path in
      let before = try openPipeCount()
      let plugin = try WasmEnginePlugin<WasmKitEngine>(path: path)
      let whileLoaded = try openPipeCount()
      #expect(whileLoaded == before)

      try plugin.shutDown()
    }
  }

  @Test
  func missingPumpShutsDownEngine() throws {
    try withPluginFile(wat2wasm(missingPump)) { path in
      let error = #expect(throws: WasmEngineError.self) {
        try WasmEnginePlugin<WasmKitEngine>(path: path)
      }

      #expect(error?.description == "Wasm plugin has an unknown ABI (could not find 'swift_wasm_macro_v1_pump')")
    }
  }

  @Test
  func missingStartShutsDownEngine() throws {
    try withPluginFile(wat2wasm(missingStart)) { path in
      let error = #expect(throws: WasmEngineError.self) {
        try WasmEnginePlugin<WasmKitEngine>(path: path)
      }

      #expect(error?.description == "Wasm plugin does not have a '_start' entrypoint")
    }
  }

  @Test
  func failedInstantiationClosesBridge() throws {
    try withPluginFile(wat2wasm(unresolvedImport)) { path in
      let error = #expect(throws: WasmKitError.self) {
        try WasmEnginePlugin<WasmKitEngine>(path: path)
      }

      #expect(error?.description == "unknown import env.missing")
    }
  }

  @Test
  func invalidModuleFails() throws {
    try withPluginFile(Array("not a Wasm module".utf8)) { path in
      let error = #expect(throws: WasmKitError.self) {
        try WasmEnginePlugin<WasmKitEngine>(path: path)
      }

      #expect(error?.description.contains("magic header not detected") == true)
    }
  }

  @Test
  func largeRequestRoundTrips() throws {
    try withPluginFile(wat2wasm(framedEcho)) { path in
      let request = [UInt8](repeating: 0x20, count: 100_000)

      let outcome = runWithTimeout(.seconds(10)) { try exchange(request, withPluginAt: path) }

      let result = try #require(outcome, "handleMessage did not return within 10 seconds")
      #expect(try result.get() == request)
    }
  }

  @Test
  func silentGuestFailsInsteadOfBlocking() throws {
    try withPluginFile(wat2wasm(emptyPlugin)) { path in
      let outcome = runWithTimeout(.seconds(10)) { try exchange([1, 2, 3], withPluginAt: path) }

      let result = try #require(outcome, "handleMessage did not return within 10 seconds")
      let error = #expect(throws: WasmEngineError.self) { try result.get() }
      #expect(error?.description == "Wasm plugin sent invalid response")
    }
  }

  @Test(arguments: [
    // A length above Int.max.
    [UInt8](repeating: 0xff, count: 8),
    // What a Wasm macro's print() writes where the length belongs.
    Array("hello, world\n".utf8),
    // A length of 10, then 3 bytes.
    [10, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3],
  ])
  func truncatedReplyFails(_ reply: [UInt8]) throws {
    try withPluginFile(wat2wasm(guestWriting(reply))) { path in
      let error = #expect(throws: WasmEngineError.self) { try exchange([1, 2, 3], withPluginAt: path) }
      #expect(error?.description == "Wasm plugin sent truncated response")
    }
  }

  @Test
  func shortReplyFailsWithoutBlocking() throws {
    try withPluginFile([10, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3]) { path in
      let plugin = try WasmEnginePlugin<BlockingReadEngine>(path: path)

      let error = #expect(throws: WasmEngineError.self) { try plugin.handleMessage([1, 2, 3]) }

      #expect(error?.description == "Wasm plugin sent truncated response")
    }
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
