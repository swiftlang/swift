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

import Synchronization
@_spi(WASIPlatform) import WASI

final class WasmStdioBuffer: WasmStdioStream {
  private struct State {
    var bytes: [UInt8] = []
    var readOffset = 0
  }

  let accessMode: FileAccessMode
  private let state = Mutex(State())

  init(accessMode: FileAccessMode) {
    self.accessMode = accessMode
  }

  /// The number of bytes in storage, including ones already read.
  var storedByteCount: Int {
    state.withLock { $0.bytes.count }
  }

  func append(_ newBytes: some Sequence<UInt8>) {
    state.withLock { $0.bytes.append(contentsOf: newBytes) }
  }

  /// Moves up to `buffer.count` of the oldest bytes into `buffer` and returns how many moved.
  func take(into buffer: UnsafeMutableRawBufferPointer) -> Int {
    state.withLock { state in
      let count = min(buffer.count, state.bytes.count - state.readOffset)
      state.bytes.withUnsafeBytes { bytes in
        buffer.copyMemory(from: UnsafeRawBufferPointer(rebasing: bytes[state.readOffset..<state.readOffset + count]))
      }
      state.readOffset += count
      if state.readOffset == state.bytes.count {
        state.bytes.removeAll()
        state.readOffset = 0
      } else if state.readOffset >= state.bytes.count - state.readOffset {
        // Compacting only once the read bytes are at least as many as the unread ones
        // keeps the total copying linear in the bytes read.
        state.bytes.removeFirst(state.readOffset)
        state.readOffset = 0
      }
      return count
    }
  }

  func read(into buffers: GuestBuffers) throws -> WASIAbi.Size {
    guard accessMode.contains(.read) else { throw WASIAbi.Errno.EBADF }
    var total: WASIAbi.Size = 0
    for index in 0..<buffers.count {
      total += WASIAbi.Size(try buffers.withHostBuffer(at: index) { take(into: $0) })
    }
    return total
  }

  func write(vectored buffers: GuestBuffers) throws -> WASIAbi.Size {
    guard accessMode.contains(.write) else { throw WASIAbi.Errno.EBADF }
    var total: WASIAbi.Size = 0
    for index in 0..<buffers.count {
      total += WASIAbi.Size(
        try buffers.withHostBuffer(at: index) { buffer in
          append(UnsafeRawBufferPointer(buffer))
          return buffer.count
        })
    }
    return total
  }
}

#endif // SWIFT_PLUGIN_SERVER_SUPPORTS_WASM
