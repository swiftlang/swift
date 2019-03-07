//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Darwin
import Foundation
@_exported import Compression

/// Compression algorithms, wraps the C API constants.
public enum Algorithm: CaseIterable {

  /// LZFSE
  case lzfse

  /// Deflate (conforming to RFC 1951)
  case zlib

  /// LZ4 with simple frame encapsulation
  case lz4

  /// LZMA in a XZ container
  case lzma

  public var rawValue: compression_algorithm {
    switch self {
    case .lzfse: return COMPRESSION_LZFSE
    case .zlib: return COMPRESSION_ZLIB
    case .lz4: return COMPRESSION_LZ4
    case .lzma: return COMPRESSION_LZMA
    }
  }
}

/// Compression errors
public enum FilterError: Error {
  /// Filter failed to initialize
  case filterInitError

  /// Invalid data in a call to compression_stream_process
  case filterProcessError

  /// Non-empty write after an output filter has been finalized
  case writeToFinalizedFilter
}

/// Compression filter direction of operation, compress/decompress
public enum FilterOperation {
  /// Compress raw data to a compressed payload
  case compress

  /// Decompress a compressed payload to raw data
  case decompress

  public var rawValue: compression_stream_operation {
    switch self {
    case .compress: return COMPRESSION_STREAM_ENCODE
    case .decompress: return COMPRESSION_STREAM_DECODE
    }
  }
}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
extension compression_stream {

  /// Initialize a compression_stream struct
  ///
  /// - Parameter operation: direction of operation
  /// - Parameter algorithm: compression algorithm
  ///
  /// - Throws: `FilterError.filterInitError` if `algorithm` is not supported
  ///           by the Compression stream API
  ///
  internal init(operation: FilterOperation, algorithm: Algorithm) throws {
    self.init(dst_ptr: UnsafeMutablePointer<UInt8>.allocate(capacity:0),
              dst_size: 0,
              src_ptr: UnsafeMutablePointer<UInt8>.allocate(capacity:0),
              src_size: 0,
              state: nil)
    let status = compression_stream_init(&self, operation.rawValue, algorithm.rawValue)
    guard status == COMPRESSION_STATUS_OK else { throw FilterError.filterInitError }
  }
}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
public class OutputFilter {
  private var _stream: compression_stream
  private var _buf: UnsafeMutablePointer<UInt8>
  private let _bufCapacity: Int
  private let _writeFunc: (Data?) throws -> ()
  private var _finalized: Bool = false

  /// Initialize an output filter
  ///
  /// - Parameters:
  /// - operation: direction of operation
  /// - algorithm: compression algorithm
  /// - bufferCapacity: capacity of the internal data buffer
  /// - writeFunc: called to write the processed data
  ///
  /// - Throws: `FilterError.StreamInitError` if stream initialization failed
  public init(
    _ operation: FilterOperation,
    using algorithm: Algorithm,
    bufferCapacity: Int = 65536,
    writingTo writeFunc: @escaping (Data?) throws -> ()
  ) throws {
    _stream = try compression_stream(operation: operation, algorithm: algorithm)
    _buf = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferCapacity)
    _bufCapacity = bufferCapacity
    _writeFunc = writeFunc
  }

  /// Send data to output filter
  ///
  /// Processed output will be sent to the output closure.
  /// A call with empty/nil data is interpreted as finalize().
  /// Writing non empty/nil data to a finalized stream is an error.
  ///
  /// - Parameter data: data to process
  ///
  /// - Throws:
  /// `FilterError.filterProcessError` if an error occurs during processing
  /// `FilterError.writeToFinalizedFilter` if `data` is not empty/nil, and the
  /// filter is the finalized state
  public func write(_ data: Data?) throws {
    // Finalize if data is empty/nil
    if data == nil || data!.isEmpty { try finalize() ; return }

    // Fail if already finalized
    if _finalized { throw FilterError.writeToFinalizedFilter }

    // Process all incoming data
    try data!.withUnsafeBytes { (src_ptr: UnsafePointer<UInt8>) in
      _stream.src_size = data!.count
      _stream.src_ptr = src_ptr
      while (_stream.src_size > 0) { _ = try process(finalizing: false) }
    }
  }

  /// Finalize the stream, i.e. flush all data remaining in the stream
  ///
  /// Processed output will be sent to the output closure.
  /// When all output has been sent, the writingTo closure is called one last time with nil data.
  /// Once the stream is finalized, writing non empty/nil data to the stream will throw an exception.
  ///
  /// - Throws: `FilterError.StreamProcessError` if an error occurs during processing
  public func finalize() throws {
    // Do nothing if already finalized
    if _finalized { return }

    // Finalize stream
    _stream.src_size = 0
    var status = COMPRESSION_STATUS_OK
    while (status != COMPRESSION_STATUS_END) { status = try process(finalizing: true) }

    // Update state
    _finalized = true

    // Notify end of stream
    try _writeFunc(nil)
  }

  // Cleanup resources.  The filter is finalized now if it was not finalized yet.
  deinit {
    // Finalize now if not done earlier
    try? finalize()

    // Cleanup
    _buf.deallocate()
    compression_stream_destroy(&_stream)
  }

  // Call compression_stream_process with current src, and dst set to _buf, then write output to the closure
  // Return status
  private func process(finalizing finalize: Bool) throws -> compression_status {
    // Process current input, and write to buf
    _stream.dst_ptr = _buf
    _stream.dst_size = _bufCapacity

    let status = compression_stream_process(&_stream, (finalize ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
    guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.filterProcessError }

    // Number of bytes written to buf
    let writtenBytes = _bufCapacity - _stream.dst_size

    // Write output
    if writtenBytes > 0 {
      let outData = Data(bytesNoCopy: _buf, count: writtenBytes, deallocator: .none)
      try _writeFunc(outData)
    }

    return status
  }

}

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
public class InputFilter {
  private var _stream: compression_stream
  private var _buf: Data? = nil // current input data
  private let _bufCapacity: Int // size to read when refilling _buf
  private let _readFunc: (Int) throws -> Data?
  private var _eofReached: Bool = false // did we read end-of-file from the input?
  private var _endReached: Bool = false // did we reach end-of-file from the decoder stream?

  /// Initialize an input filter
  ///
  /// - Parameters:
  /// - operation: direction of operation
  /// - algorithm: compression algorithm
  /// - bufferCapacity: capacity of the internal data buffer
  /// - readFunc: called to read the input data
  ///
  /// - Throws: `FilterError.filterInitError` if filter initialization failed
  public init(
    _ operation: FilterOperation,
    using algorithm: Algorithm,
    bufferCapacity: Int = 65536,
    readingFrom readFunc: @escaping (Int) throws -> Data?
  ) throws {
    _stream = try compression_stream(operation: operation, algorithm: algorithm)
    _bufCapacity = bufferCapacity
    _readFunc = readFunc
  }

  /// Read processed data from the filter
  ///
  /// Input data, when needed, is obtained from the input closure
  /// When the input closure returns a nil or empty Data object, the filter will be
  /// finalized, and after all processed data has been read, readData will return nil
  /// to signal end of input
  ///
  /// - Parameter count: max number of bytes to read from the filter
  ///
  /// - Returns: a new Data object containing at most `count` output bytes, or nil if no more data is available
  ///
  /// - Throws:
  /// `FilterError.filterProcessError` if an error occurs during processing
  public func readData(ofLength count: Int) throws -> Data? {
    // Sanity check
    precondition(count > 0, "number of bytes to read can't be 0")

    // End reached, return early, nothing to do
    if _endReached { return nil }

    // Allocate result
    var result = Data(count: count)

    try result.withUnsafeMutableBytes { (dst_ptr: UnsafeMutablePointer<UInt8>) in

      // Write to result until full, or end reached
      _stream.dst_size = count
      _stream.dst_ptr = dst_ptr

      while _stream.dst_size > 0 && !_endReached {

        // Refill _buf if needed, and EOF was not yet read
        if _stream.src_size == 0 && !_eofReached {
          _buf = try _readFunc(_bufCapacity) // may be nil
          // Reset src_size to full _buf size
          if _buf?.count ?? 0 == 0 { _eofReached = true }
          _stream.src_size = _buf?.count ?? 0
        }

        // Process some data
        if let buf = _buf {
          try buf.withUnsafeBytes { (src_ptr: UnsafePointer<UInt8>) in

            // Next byte to read
            _stream.src_ptr = src_ptr + buf.count - _stream.src_size

            let status = compression_stream_process(&_stream, (_eofReached ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
            guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.filterProcessError }
            if status == COMPRESSION_STATUS_END { _endReached = true }
          }
        }
        else {
          let status = compression_stream_process(&_stream, (_eofReached ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
          guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.filterProcessError }
          if status == COMPRESSION_STATUS_END { _endReached = true }

        }
      }

    } // result.withUnsafeMutableBytes

    // Update actual size
    result.count = count - _stream.dst_size
    return result
  }

  // Cleanup resources
  deinit {
    compression_stream_destroy(&_stream)
  }

}
