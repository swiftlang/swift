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
  /// Creates a `String` whose backing store is shared with the UTF-8 data
  /// referenced by the given buffer pointer.
  ///
  /// The `owner` argument should manage the lifetime of the shared buffer and
  /// its deinitializer is responsible for deallocating `buffer`. The `String`
  /// instance created by this initializer retains `owner` so that deallocation
  /// occurs after the string is no longer in use. The buffer _must not_ be
  /// deallocated while there are any strings sharing it.
  ///
  /// This initializer does not try to repair ill-formed UTF-8 code unit
  /// sequences. If any are found, the result of the initializer is `nil`.
  ///
  /// - Parameters:
  ///   - buffer: An `UnsafeBufferPointer` containing the UTF-8 bytes that
  ///     should be shared with the created `String`.
  ///   - owner: An optional object that owns the memory referenced by `buffer`
  ///     and is responsible for deallocating it.
  public init?(
    sharingContent buffer: UnsafeBufferPointer<UInt8>,
    owner: AnyObject?
  ) {
    guard let baseAddress = buffer.baseAddress,
      case .success(let extraInfo) = validateUTF8(buffer)
    else {
      return nil
    }
    let storage = __SharedStringStorage(
      immortal: baseAddress,
      countAndFlags: _StringObject.CountAndFlags(
        sharedCount: buffer.count,
        isASCII: extraInfo.isASCII))
    storage._owner = owner
    self.init(String(_StringGuts(storage)))
  }
}

extension Substring {

}
