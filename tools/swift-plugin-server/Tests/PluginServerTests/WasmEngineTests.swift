@testable import swift_plugin_server
import Foundation
import SystemPackage
import Testing
import WasmKit
import WasmKitWASI
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

@Suite
struct WasmEngineTests {
  @Test
  func basic() throws {
    let binary = try wat2wasm(echo)
    let echoURL = FileManager.default.temporaryDirectory.appendingPathExtension("echo.wasm")
    try Data(binary).write(to: echoURL)
    defer { try! FileManager.default.removeItem(at: echoURL) }

    let wasmKit = try WasmEnginePlugin<WasmKitEngine>(path: FilePath(echoURL.path))
    let input: [UInt8] = [1,2,3,4,5]
    #expect(try wasmKit.handleMessage(input) == input)
  }
}
