//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// String Creation Helpers
//===----------------------------------------------------------------------===//

internal func _allASCII(_ input: UnsafeRawBufferPointer) -> Bool {
  // NOTE: Avoiding for-in syntax to avoid bounds checks
  //
  // TODO(String performance): SIMD-ize
  //
  let ptr = input.baseAddress._unsafelyUnwrappedUnchecked
  var i = 0

  let count = input.count
  let align = MemoryLayout<UInt>.alignment
  let stride = MemoryLayout<UInt>.stride
  let address = Int(bitPattern: ptr)

  let wordASCIIMask = UInt(truncatingIfNeeded: 0x8080_8080_8080_8080 as UInt64)
  let byteASCIIMask = UInt8(truncatingIfNeeded: wordASCIIMask)

  while (address &+ i) % align != 0 && i < count {
    guard ptr.load(fromByteOffset: i, as: UInt8.self) & byteASCIIMask == 0 
      else { return false }
    i &+= 1
  }

  while (i &+ stride) <= count {
    guard ptr.load(fromByteOffset: i, as: UInt.self) & wordASCIIMask == 0 
      else { return false }
    i &+= stride
  }

  while i < count {
    guard ptr.load(fromByteOffset: i, as: UInt8.self) & byteASCIIMask == 0 
      else { return false }
    i &+= 1
  }
  return true
}

internal func _allASCII(_ input: UnsafeBufferPointer<UInt8>) -> Bool {
  return _allASCII(UnsafeRawBufferPointer(input))
}

extension String {
  @usableFromInline
  internal static func _fromASCII(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    _internalInvariant(_allASCII(input), "not actually ASCII")

    if let smol = _SmallString(input) {
      return String(_StringGuts(smol))
    }

    let storage = __StringStorage.create(initializingFrom: input, isASCII: true)
    return storage.asString
  }

  // UTF8 from typed memory.

  public // SPI(Foundation)
  static func _tryFromUTF8(_ input: UnsafeBufferPointer<UInt8>) -> String? {
    return _tryFromUTF8(UnsafeRawBufferPointer(input))
  }

  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    return _uncheckedFromUTF8(UnsafeRawBufferPointer(input),
                              isASCII: _allASCII(input))
  }

  @usableFromInline
  internal static func _fromUTF8Repairing(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> (result: String, repairsMade: Bool) {
    return _fromUTF8Repairing(UnsafeRawBufferPointer(input))
  }

  // UTF8 from raw bytes.

  public // SPI(Foundation)
  static func _tryFromUTF8(_ input: UnsafeRawBufferPointer) -> String? {
    guard case .success(let extraInfo) = validateUTF8(input) else {
      return nil
    }

    return String._uncheckedFromUTF8(input, isASCII: extraInfo.isASCII)
  }

  @usableFromInline
  internal static func _fromUTF8Repairing(
    _ input: UnsafeRawBufferPointer
  ) -> (result: String, repairsMade: Bool) {
    switch validateUTF8(input) {
    case .success(let extraInfo):
      return (String._uncheckedFromUTF8(input, isASCII: extraInfo.isASCII),
              false)
    case .error(let initialRange):
      return (repairUTF8(input, firstKnownBrokenRange: initialRange), true)
    }
  }

  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeRawBufferPointer,
    isASCII: Bool
  ) -> String {
    if let smol = _SmallString(input) {
      return String(_StringGuts(smol))
    }

    let storage = __StringStorage.create(
      initializingFrom: input, isASCII: isASCII)
    return storage.asString
  }

  // If we've already pre-scanned for ASCII, just supply the result
  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>, asciiPreScanResult: Bool
  ) -> String {
    return _uncheckedFromUTF8(UnsafeRawBufferPointer(input),
                              isASCII: asciiPreScanResult)
  }

  // Other encodings.

  @usableFromInline
  internal static func _uncheckedFromUTF16(
    _ input: UnsafeBufferPointer<UInt16>
  ) -> String {
    // TODO(String Performance): Attempt to form smol strings

    // TODO(String performance): Skip intermediary array, transcode directly
    // into a StringStorage space.
    var contents: [UInt8] = []
    contents.reserveCapacity(input.count)
    let repaired = transcode(
      input.makeIterator(),
      from: UTF16.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { contents.append($0) })
    _internalInvariant(!repaired, "Error present")

    return contents.withUnsafeBufferPointer { String._uncheckedFromUTF8($0) }
  }

  internal func _withUnsafeBufferPointerToUTF8<R>(
    _ body: (UnsafeBufferPointer<UTF8.CodeUnit>) throws -> R
  ) rethrows -> R {
    return try self._withUTF8(body)
  }

  @usableFromInline @inline(never) // slow-path
  internal static func _fromCodeUnits<
    Input: Collection,
    Encoding: Unicode.Encoding
  >(
    _ input: Input,
    encoding: Encoding.Type,
    repair: Bool
  ) -> (String, repairsMade: Bool)?
  where Input.Element == Encoding.CodeUnit {
    // TODO(String Performance): Attempt to form smol strings

    // TODO(String performance): Skip intermediary array, transcode directly
    // into a StringStorage space.
    var contents: [UInt8] = []
    contents.reserveCapacity(input.underestimatedCount)
    let repaired = transcode(
      input.makeIterator(),
      from: Encoding.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { contents.append($0) })
    guard repair || !repaired else { return nil }

    let str = contents.withUnsafeBufferPointer { String._uncheckedFromUTF8($0) }
    return (str, repaired)
  }

  public // @testable
  static func _fromInvalidUTF16(
    _ utf16: UnsafeBufferPointer<UInt16>
  ) -> String {
    return String._fromCodeUnits(utf16, encoding: UTF16.self, repair: true)!.0
  }

  @usableFromInline
  internal static func _fromSubstring(
    _ substring: __shared Substring
  ) -> String {
    if substring._offsetRange == substring._wholeString._offsetRange {
      return substring._wholeString
    }

    return substring._withUTF8 { return String._uncheckedFromUTF8($0) }
  }
}
