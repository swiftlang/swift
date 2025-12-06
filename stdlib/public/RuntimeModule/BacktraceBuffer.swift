//===--- BacktraceBuffer.swift --------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  A ring buffer used for capturing multiple backtraces; the expected use
//  case for this is in-process profiling, so it is important that all the
//  memory be allocated up-front as no allocation can take place during
//  backtrace capture itself.
//
//===----------------------------------------------------------------------===//

import Swift

/// `BacktraceBuffer` is used to repeatedly capture backtraces; it calls
/// a user-defined closure whenever it runs out of space in the buffer,
/// in the expectation that the closure will empty data out of the buffer.
///
/// To use this class, you would create a `BacktraceBuffer` in advance,
/// giving the various capture arguments at that time, then you call
/// the `capture()` method repeatedly, and finally when you are done,
/// you call `flush()`; something like this:
///
///     let fd = open("/tmp/captured.bin", O_RDWR, 0644)
///     let buffer = BacktraceBuffer(
///       capacity: 65536,
///       algorithm: .auto,
///       limit: 64,
///       offset: 0,
///       top: 16
///     ) { chunk in
///       return write(fd, chunk.baseAddress, chunk.count)
///     }
///     defer {
///       buffer.flush()
///       close(fd)
///     }
///
///     let reader = UnsafeLocalMemoryReader()
///
///     for threadContext in capturedThreadContexts {
///       buffer.capture(from: threadContext, using: reader)
///     }
@_spi(BacktraceBuffer)
public class BacktraceBuffer<C: Context, M: MemoryReader> {
  public typealias Context = C
  public typealias MemoryReader = M
  public typealias Address = Context.Address
  public typealias UnwindAlgorithm = Backtrace.UnwindAlgorithm

  public private(set) var reader: MemoryReader

  var storage: UnsafeMutableRawBufferPointer

  public private(set) var read, write, count: Int

  public private(set) var algorithm: UnwindAlgorithm
  public private(set) var limit: Int?
  public private(set) var offset, top: Int
  public private(set) var images: ImageMap?

  var topBuffer: UnsafeMutableBufferPointer<RichFrame<Address>>

  public private(set) var processChunk: (UnsafeRawBufferPointer) throws -> Int

  /// Construct a BacktraceBuffer.
  ///
  /// This constructor allocates memory that will then be used whenever
  /// you call `capture()`.
  ///
  /// Parameters:
  ///
  /// - using:         The memory reader to use.
  /// - capacity:      The size of the buffer to use, in bytes.
  /// - algorithm:     The unwind algorithm to use.
  /// - limit:         The maximum number of frames to capture for each call
  ///                  of `capture()`.
  /// - offset:        The number of frames to skip for each call of `capture()`.
  /// - top:           The minimum number of frames to capture at the top of
  ///                  the stack.
  /// - images:        (Optional) A list of captured images.
  /// - processChunk:  Called whenever the buffer fills up to; should
  ///                  return the number of bytes from the buffer it has
  ///                  processed.
  public init(using reader: MemoryReader,
              capacity: Int = 65536, algorithm: UnwindAlgorithm = .auto,
              limit: Int? = 64, offset: Int = 0, top: Int = 16,
              images: ImageMap? = nil,
              processChunk: @escaping (UnsafeRawBufferPointer) throws -> Int) {
    self.reader = reader

    storage = .allocate(byteCount: capacity, alignment: 4)
    read = 0
    write = 0
    count = 0

    self.algorithm = algorithm
    self.limit = limit
    self.offset = offset
    self.top = top

    topBuffer = .allocate(capacity: top)

    self.processChunk = processChunk

    #if os(Linux)
    // On Linux, we need the captured images to resolve async functions
    self.images = images ?? ImageMap.capture()
    #else
    self.images = images
    #endif
  }

  deinit {
    storage.deallocate()
    topBuffer.deallocate()
  }

  /// Capture a backtrace into the buffer.
  ///
  /// Parameters:
  ///
  /// - from:          The `Context` the backtrace is to start from.
  public func capture(
    from context: Context,
  ) throws {
    switch algorithm {
      // All algorithms, for now, use the frame pointer unwinder.  In the
      // long run, we should be using DWARF EH for .precise.
      default:
        let unwinder =
          FramePointerUnwinder(context: context,
                               images: images,
                               memoryReader: reader)

        if let limit {
          let limited = NonAllocatingLimitSequence(unwinder,
                                                   limit: limit,
                                                   offset: offset,
                                                   top: top,
                                                   topBuffer: topBuffer)

          let encoder = CompactBacktraceFormat.Encoder(limited)

          try fillBuffer(source: encoder)
        } else {
          let encoder = CompactBacktraceFormat.Encoder(unwinder)

          try fillBuffer(source: encoder)
        }
    }
  }

  /// Flush any data left in the buffer.
  ///
  /// You should call this when you have finished capturing data.
  ///
  /// Parameters:
  ///
  /// - processChunk:  Called for each remaining chunk of data in the buffer.
  ///                  (Generally there will be at most two such chunks.)
  public func flush() throws {
    // Empty the buffer completely
    while count > 0 {
      let chunkSize = Swift.min(storage.count - read, count)
      let chunkEnd = read + chunkSize
      let chunk = UnsafeRawBufferPointer(
        rebasing: storage[read..<chunkEnd]
      )
      let done = try processChunk(chunk)

      assert(done > 0 && done <= chunkSize)

      count -= done
      read += done

      if read >= storage.count {
        read -= storage.count
      }
    }
  }

  /// Fill the buffer with data from the specified byte source.
  ///
  /// Parameters:
  ///
  /// - source: The byte source to use to fill the buffer.
  func fillBuffer<S: Sequence<UInt8>>(source: S) throws {
    for byte in source {
      storage[write] = byte
      write += 1
      count += 1

      if write == storage.count {
        write = 0
      }

      if count == storage.count {
        // Try to clear some space in the buffer
        let chunkSize = Swift.min(storage.count - read, count)
        let chunkEnd = read + chunkSize
        let chunk = UnsafeRawBufferPointer(
          rebasing: storage[read..<chunkEnd]
        )
        let done = try processChunk(chunk)

        assert(done > 0 && done <= chunkSize)

        count -= done
        read += done

        if read >= storage.count {
          read -= storage.count
        }
      }
    }
  }
}
