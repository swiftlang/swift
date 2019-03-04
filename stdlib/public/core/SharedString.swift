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

extension String {
  /// Calls the given closure with a `String` whose backing store is shared with
  /// the UTF-8 data referenced by the given buffer pointer.
  ///
  /// This method does not try to repair ill-formed UTF-8 code unit sequences.
  /// If any are found, the closure parameter is `nil`.
  ///
  /// - Parameters:
  ///   - buffer: An `UnsafeBufferPointer` containing the UTF-8 bytes that
  ///     should be shared with the created `String`.
  ///   - body: A closure with an optional `String` parameter that shares the
  ///     memory pointed to by `buffer`. The string argument is valid only for
  ///     the duration of the method's execution, and it may be nil if the
  ///     `String` could not be created (for example, if the bytes were not
  ///     valid UTF-8).
  /// - Returns: The return value, if any, of the `body` closure parameter.
  public static func withSharedUTF8<Result>(
    _ buffer: UnsafeBufferPointer<UInt8>,
    _ body: (String?) throws -> Result
  ) rethrows -> Result {
    return try body(String(sharingUTF8: buffer, deallocator: { _ in }))
  }

  /// Calls the given closure with a `String` whose backing store is shared with
  /// the null-terminated UTF-8 data referenced by the given pointer.
  ///
  /// This method does not try to repair ill-formed UTF-8 code unit sequences.
  /// If any are found, the closure parameter is `nil`.
  ///
  /// - Parameters:
  ///   - cString: An `UnsafePointer` containing the null-terminated UTF-8 bytes
  ///     that should be shared with the created `String`.
  ///   - body: A closure with an optional `String` parameter that shares the
  ///     memory pointed to by `buffer`. The string argument is valid only for
  ///     the duration of the method's execution, and it may be nil if the
  ///     `String` could not be created (for example, if the bytes were not
  ///     valid UTF-8).
  /// - Returns: The return value, if any, of the `body` closure parameter.
  public static func withSharedNullTerminatedUTF8<Result>(
    _ cString: UnsafePointer<UInt8>,
    _ body: (String?) throws -> Result
  ) rethrows -> Result {
    let len = UTF8._nullCodeUnitOffset(in: cString)
    let buf = UnsafeBufferPointer<UInt8>(start: cString, count: len)
    return try withSharedUTF8(buf, body)
  }
}

extension String {
  internal class SharedDeallocatingOwner {
    internal let pointer: UnsafePointer<UInt8>
    internal let deallocator: (UnsafePointer<UInt8>) -> Void

    internal init(
      pointer: UnsafePointer<UInt8>,
      deallocator: @escaping (UnsafePointer<UInt8>) -> Void
    ) {
      self.pointer = pointer
      self.deallocator = deallocator
    }

    deinit {
      deallocator(pointer)
    }
  }

  /// Creates a `String` whose backing store is shared with the UTF-8 data
  /// referenced by the given buffer pointer, which will be released by calling
  /// `deallocate`.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// - Parameter buffer: An `UnsafeBufferPointer` containing the UTF-8 bytes
  ///   that should be shared with the created `String`.
  public init?(sharingUTF8 buffer: UnsafeBufferPointer<UInt8>) {
    self.init(
      sharingUTF8: buffer, deallocator: { ptr in ptr.deallocate() })
  }

  /// Creates a `String` whose backing store is shared with the UTF-8 data
  /// referenced by the given buffer pointer, which will be released by calling
  /// the `deallocator` closure.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// - Parameters:
  ///   - buffer: An `UnsafeBufferPointer` containing the UTF-8 bytes that
  ///     should be shared with the created `String`.
  ///   - deallocator: A closure called to free the backing store at the end of
  ///     the string's lifetime.
  public init?(
    sharingUTF8 buffer: UnsafeBufferPointer<UInt8>,
    deallocator: @escaping (UnsafePointer<UInt8>) -> Void
  ) {
    guard
      let baseAddress = buffer.baseAddress,
      case .success(let extraInfo) = validateUTF8(buffer)
    else {
      return nil
    }
    let storage = __SharedStringStorage(
      immortal: baseAddress,
      countAndFlags: _StringObject.CountAndFlags(
        sharedCount: buffer.count,
        isASCII: extraInfo.isASCII))
    storage._owner = SharedDeallocatingOwner(
      pointer: baseAddress, deallocator: deallocator)
    self.init(String(_StringGuts(storage)))
  }

  /// Creates a `String` whose backing store is shared with the null-terminated
  /// UTF-8 data referenced by the given buffer pointer, which will be released
  /// by calling `deallocate`.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// - Parameter cString: An `UnsafePointer` containing the null-terminated
  ///   UTF-8 bytes that should be shared with the created `String`.
  public init?(sharingNullTerminatedUTF8 cString: UnsafePointer<UInt8>) {
    self.init(
      sharingNullTerminatedUTF8: cString,
      deallocator: { ptr in ptr.deallocate() })
  }

  /// Creates a `String` whose backing store is shared with the null-terminated
  /// UTF-8 data referenced by the given pointer, which will be released by
  /// calling the `deallocator` closure.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// - Parameters:
  ///   - cString: An `UnsafePointer` containing the null-terminated UTF-8 bytes
  ///     that should be shared with the created `String`.
  ///   - deallocator: A closure called to free the backing store at the end of
  ///     the string's lifetime.
  public init?(
    sharingNullTerminatedUTF8 cString: UnsafePointer<UInt8>,
    deallocator: @escaping (UnsafePointer<UInt8>) -> Void
  ) {
    let len = UTF8._nullCodeUnitOffset(in: cString)
    let buf = UnsafeBufferPointer<UInt8>(start: cString, count: len)
    self.init(sharingUTF8: buf, deallocator: deallocator)
  }
}
