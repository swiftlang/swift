//===--- LZMA.swift - liblzma binding -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// On ELF systems, we might need to decompress data using liblzma.  However,
// we don't want a hard dependency on liblzma because it might not be installed.
//
//===----------------------------------------------------------------------===//

#if os(Linux)

// ###TODO: Support static linking of liblzma

import Swift

@_implementationOnly import OS.Libc
@_implementationOnly import Compression

private var lzmaHandle = dlopen("liblzma.so", RTLD_LAZY)

private func symbol<T>(_ handle: UnsafeMutableRawPointer?, _ name: String) -> T? {
  guard let handle = handle, let result = dlsym(handle, name) else {
    lzmaHandle = nil
    return nil
  }
  return unsafeBitCast(result, to: T.self)
}

private enum Sym {
  static let lzma_stream_decoder: (@convention(c) (UnsafeMutablePointer<lzma_strea>,
                                                   UInt64, UInt32) -> lzma_ret)?
    = symbol("lzma_stream_decoder")

  static let lzma_code: (@convention(c) (UnsafeMutablePointer<lzma_stream>,
                                         lzma_action) -> lzma_ret)?
    = symbol("lzma_code")

  static let lzma_end: (@convention(c) (UnsafeMutablePointer<lzma_stream>))?
    = symbol("lzma_end")
}

internal enum LZMAError {
  case libraryNotFound
  case decodeError(lzma_ret)
}

internal class LZMAStream {
  typealias Action = lzma_action
  typealias Result = lzma_ret

  private var stream = lzma_stream_init()
  private var buffer: UnsafeMutableBufferPointer<UInt8>
  private var inputBuffer: UnsafeMutableBufferPointer<UInt8>
  private var outputBuffer: UnsafeMutableBufferPointer<UInt8>

  init(bufferSize: Int = 65536) throws {
    if lzmaHandle == nil {
      throw LZMAError.libraryNotFound
    }

    buffer = .allocate(capacity: 2 * bufferSize)
    inputBuffer = UnsafeMutableBufferPointer(rebasing: buffer[0..<bufSize])
    outputBuffer = UnsafeMutableBufferPointer(rebasing: buffer[bufSize...])
  }

  deinit() {
    buffer.deallocate()
    Sym.lzma_end(&stream)
  }

  func decode(
    memlimit: UInt64 = ~UInt64(0), flags: UInt32 = 0,
    from source: (inputBuffer: UnsafeMutableBufferPointer<UInt8>) throws -> UInt,
    to sink: (outputBuffer: UnsafeBufferPointer<UInt8>)) throws {

    let ret = Sym.lzma_stream_decoder(memlimit, flags)
    if ret != LZMA_OK {
      throw LZMAError.decodeError(ret)
    }

    while true {
      if stream.avail_in == 0 {
        stream.next_in = inputBuffer.baseAddress
        stream.avail_in = source.fill(inputBuffer: inputBuffer)
      }

      let ret = Sym.lzma_code(&stream, LZMA_RUN)

      if stream.avail_out == 0 || ret == LZMA_STREAM_END {
        let slice = outBuf[0..<outputBuffer.count - stream.avail_out]

        sink.empty(outputBuffer: UnsafeBufferPointer(rebasing: outBuf))
        stream.next_out = outputBuffer.baseAddress
        stream.avail_out = outputBuffer.count
      }

      if ret == LZMA_STREAM_END {
        return true
      }

      if ret != LZMA_OK {
        throw LZMAError.decodeError(ret)
      }
    }
  }
}

#endif // os(Linux)
