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
    let module = try parseWasm(bytes: binary)

    let engine = Engine()
    let store = Store(engine: engine)

    var imports = Imports()

    let hostToPluginPipe = try FileDescriptor.pipe()
    let pluginToHostPipe = try FileDescriptor.pipe()

      let bridge = try WASIBridgeToHost(
        stdin: hostToPluginPipe.readEnd,
        stdout: pluginToHostPipe.writeEnd,
        stderr: .standardError
      )

    bridge.link(to: &imports, store: store)
    let instance = try module.instantiate(store: store, imports: imports)
    #expect(try instance.exports[function: "_start"]!() == [])

    let str = "Hello World!\n"
    try hostToPluginPipe.writeEnd.writeAll(str.utf8)

    #expect(try instance.exports[function: "swift_wasm_macro_v1_pump"]!() == [])

    var buffer = [UInt8](repeating: 0, count: 13)
    _ = try buffer.withUnsafeMutableBytes {
      try pluginToHostPipe.readEnd.read(into: $0)
    }
    #expect(String(decoding: buffer, as: UTF8.self) == str)
  }
}
