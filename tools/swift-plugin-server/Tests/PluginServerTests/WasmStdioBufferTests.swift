//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_PLUGIN_SERVER_SUPPORTS_WASM

@testable import swift_plugin_server
import Testing
@_spi(WASIPlatform) import WASI

@Suite
struct WasmStdioBufferTests {
  @Test
  func takeReleasesReadBytes() {
    let buffer = WasmStdioBuffer(accessMode: .read)
    var received: [UInt8] = []

    for round in 0..<3 {
      buffer.append((0..<10).map { UInt8(round * 10 + $0) })
      var chunk = [UInt8](repeating: 0, count: 9)
      let moved = chunk.withUnsafeMutableBytes { buffer.take(into: $0) }
      received += chunk[..<moved]
    }

    #expect(received == Array(0..<27))
    #expect(buffer.storedByteCount == 3)
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
