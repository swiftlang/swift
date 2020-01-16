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
@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public enum Algorithm: CaseIterable, RawRepresentable {

  /// LZFSE
  case lzfse

  /// Deflate (conforming to RFC 1951)
  case zlib

  /// LZ4 with simple frame encapsulation
  case lz4

  /// LZMA in a XZ container
  case lzma

  public init?(rawValue: compression_algorithm) {
    switch rawValue {
    case COMPRESSION_LZFSE: self = .lzfse
    case COMPRESSION_ZLIB: self = .zlib
    case COMPRESSION_LZ4: self = .lz4
    case COMPRESSION_LZMA: self = .lzma
    default: return nil
    }
  }

  public var rawValue: compression_algorithm {
    switch self {
    case .lzfse: return COMPRESSION_LZFSE
    case .zlib: return COMPRESSION_ZLIB
    case .lz4: return COMPRESSION_LZ4
    case .lzma: return COMPRESSION_LZMA
    }
  }
}

/// Compression filter direction of operation, compress/decompress
@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public enum FilterOperation: RawRepresentable {

  /// Compress raw data to a compressed payload
  case compress

  /// Decompress a compressed payload to raw data
  case decompress

  public init?(rawValue: compression_stream_operation) {
    switch rawValue {
    case COMPRESSION_STREAM_ENCODE: self = .compress
    case COMPRESSION_STREAM_DECODE: self = .decompress
    default: return nil
    }
  }

  public var rawValue: compression_stream_operation {
    switch self {
    case .compress: return COMPRESSION_STREAM_ENCODE
    case .decompress: return COMPRESSION_STREAM_DECODE
    }
  }
}

/// Compression errors
@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public enum FilterError: Error {

  /// Filter failed to initialize,
  /// or invalid internal state,
  /// or invalid parameters
  case invalidState

  /// Invalid data in a call to compression_stream_process,
  /// or non-empty write after an output filter has been finalized
  case invalidData
}

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
extension compression_stream {

  /// Initialize a compression_stream struct
  ///
  /// - Parameter operation: direction of operation
  /// - Parameter algorithm: compression algorithm
  ///
  /// - Throws: `FilterError.invalidState` if `algorithm` is not supported
  ///           by the Compression stream API
  ///
  internal init(operation: FilterOperation, algorithm: Algorithm) throws {
    self.init(dst_ptr: UnsafeMutablePointer<UInt8>(bitPattern: -1)!,
              dst_size: 0,
              src_ptr: UnsafeMutablePointer<UInt8>(bitPattern: -1)!,
              src_size: 0,
              state: nil)
    let status = compression_stream_init(&self, operation.rawValue, algorithm.rawValue)
    guard status == COMPRESSION_STATUS_OK else { throw FilterError.invalidState }
  }
}

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public class OutputFilter {
  private var _stream: compression_stream
  private var _buf: UnsafeMutablePointer<UInt8>
  private let _bufCapacity: Int
  private let _writeFunc: (Data?) throws -> Void
  private var _finalized: Bool = false

  /// Initialize an output filter
  ///
  /// - Parameters:
  /// - operation: direction of operation
  /// - algorithm: compression algorithm
  /// - bufferCapacity: capacity of the internal data buffer
  /// - writeFunc: called to write the processed data
  ///
  /// - Throws: `FilterError.invalidState` if stream initialization failed
  public init(
    _ operation: FilterOperation,
    using algorithm: Algorithm,
    bufferCapacity: Int = 65536,
    writingTo writeFunc: @escaping (Data?) throws -> Void
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
  /// `FilterError.invalidData` if an error occurs during processing,
  /// or if `data` is not empty/nil, and the filter is the finalized state
  public func write<D : DataProtocol>(_ data: D?) throws {
    // Finalize if data is empty/nil
    if data == nil || data!.isEmpty { try finalize() ; return }

    // Fail if already finalized
    if _finalized { throw FilterError.invalidData }

    // Process all incoming data
    for region in data!.regions {
      try region.withUnsafeBytes { (raw_src_ptr: UnsafeRawBufferPointer) in
        _stream.src_size = region.count
        _stream.src_ptr = raw_src_ptr.baseAddress!.assumingMemoryBound(to: UInt8.self)
        while (_stream.src_size > 0) { _ = try _process(finalizing: false) }
      }
    }
  }

  /// Finalize the stream, i.e. flush all data remaining in the stream
  ///
  /// Processed output will be sent to the output closure.
  /// When all output has been sent, the writingTo closure is called one last time with nil data.
  /// Once the stream is finalized, writing non empty/nil data to the stream will throw an exception.
  ///
  /// - Throws: `FilterError.invalidData` if an error occurs during processing
  public func finalize() throws {
    // Do nothing if already finalized
    if _finalized { return }

    // Finalize stream
    _stream.src_size = 0
    var status = COMPRESSION_STATUS_OK
    while (status != COMPRESSION_STATUS_END) { status = try _process(finalizing: true) }

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
  private func _process(finalizing finalize: Bool) throws -> compression_status {
    // Process current input, and write to buf
    _stream.dst_ptr = _buf
    _stream.dst_size = _bufCapacity

    let status = compression_stream_process(&_stream, (finalize ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
    guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.invalidData }

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

@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
public class InputFilter<D: DataProtocol> {

  // Internal buffer to read bytes from a DataProtocol implementation
  private class InputFilterBuffer<D: DataProtocol> {
    private var _data: D // current input data
    private var _remaining: Int // total bytes remaining to process in _data
    private var _regionIndex: D.Regions.Index // region being read in _data
    private var _regionRemaining: Int // remaining bytes to read in region being read in _data

    public init(_ data: D) throws {
      _data = data
      _remaining = _data.count
      _regionRemaining = 0
      _regionIndex = _data.regions.startIndex
      if _regionIndex != _data.regions.endIndex { _regionRemaining = _data.regions[_regionIndex].count }
      // Advance to first non-zero region
      try advance(by: 0)
    }

    // Return number of remaining bytes
    public func remaining() -> Int { return _remaining }

    // Call f with a buffer to the remaining bytes of the current contiguous region
    public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R? {
      if _remaining == 0 {
        return try body(UnsafeRawBufferPointer(start: nil, count: 0))
      } else {
        let r = _data.regions[_regionIndex]
        return try r.withUnsafeBytes { (region_buf: UnsafeRawBufferPointer) in
          let src_buf = UnsafeRawBufferPointer(
            start: region_buf.baseAddress!.assumingMemoryBound(to: UInt8.self) + region_buf.count - _regionRemaining,
            count: _regionRemaining)
          return try body(src_buf)
        }
      }
    }

    // Consume n bytes in the current region (n can be 0, up to _regionRemaining), and move to next non-empty region if needed
    // post-condition: _remaining == 0 || _regionRemaining > 0
    public func advance(by n: Int) throws {

      // Sanity checks
      if n > _regionRemaining { throw FilterError.invalidState } // invalid n

      // Update counters
      _regionRemaining -= n
      _remaining -= n

      // Move to next non-empty region if we are done with the current one
      let r = _data.regions
      while _regionRemaining == 0 {
        r.formIndex(after: &_regionIndex)
        if _regionIndex == r.endIndex { break }
        _regionRemaining = r[_regionIndex].count
      }

      // Sanity checks
      if _remaining != 0 && _regionRemaining == 0 { throw FilterError.invalidState }
    }
  }

  private let _readCapacity: Int // size to use when calling _readFunc
  private let _readFunc: (Int) throws -> D? // caller-provided read function
  private var _stream: compression_stream
  private var _buf: InputFilterBuffer<D>? = nil // input
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
  /// - Throws: `FilterError.invalidState` if filter initialization failed
  public init(
    _ operation: FilterOperation,
    using algorithm: Algorithm,
    bufferCapacity: Int = 65536,
    readingFrom readFunc: @escaping (Int) throws -> D?
  ) throws {
    _stream = try compression_stream(operation: operation, algorithm: algorithm)
    _readCapacity = bufferCapacity
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
  /// `FilterError.invalidData` if an error occurs during processing
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
        if (_buf?.remaining() ?? 0) == 0 && !_eofReached {
          let data = try _readFunc(_readCapacity) // may be nil, or empty
          if data?.count ?? 0 == 0 { // nil or empty -> EOF
            _eofReached = true
            _buf = nil
          } else {
            _buf = try InputFilterBuffer(data!)
          }
        }

        // Process some data
        if let buf = _buf {
          try buf.withUnsafeBytes { (src_buf: UnsafeRawBufferPointer) in
            // Point to buffer
            _stream.src_ptr = src_buf.baseAddress!.assumingMemoryBound(to: UInt8.self)
            _stream.src_size = src_buf.count
            let status = compression_stream_process(&_stream, (_eofReached ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
            guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.invalidData }
            if status == COMPRESSION_STATUS_END { _endReached = true }
            // Advance by the number of consumed bytes
            let consumed = src_buf.count - _stream.src_size
            try buf.advance(by: consumed)
          }
        } else {
          // No data available, process until END reached
          let status = compression_stream_process(&_stream, (_eofReached ? Int32(COMPRESSION_STREAM_FINALIZE.rawValue) : 0))
          guard status != COMPRESSION_STATUS_ERROR else { throw FilterError.invalidData }
          if status == COMPRESSION_STATUS_END { _endReached = true }
        }

      } // _stream.dst_size > 0 && !_endReached

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
