//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// String Creation Helpers
//===----------------------------------------------------------------------===//

internal func _allASCII(_ input: UnsafeBufferPointer<UInt8>) -> Bool {
  if input.isEmpty { return true }

  // NOTE: Avoiding for-in syntax to avoid bounds checks
  //
  // TODO(String performance): SIMD-ize
  //
  let count = input.count
  var ptr = unsafe UnsafeRawPointer(input.baseAddress._unsafelyUnwrappedUnchecked)

  let asciiMask64 = 0x8080_8080_8080_8080 as UInt64
  let asciiMask32 = UInt32(truncatingIfNeeded: asciiMask64)
  let asciiMask16 = UInt16(truncatingIfNeeded: asciiMask64)
  let asciiMask8 = UInt8(truncatingIfNeeded: asciiMask64)
  
  let end128 = unsafe ptr + count & ~(MemoryLayout<(UInt64, UInt64)>.stride &- 1)
  let end64 = unsafe ptr + count & ~(MemoryLayout<UInt64>.stride &- 1)
  let end32 = unsafe ptr + count & ~(MemoryLayout<UInt32>.stride &- 1)
  let end16 = unsafe ptr + count & ~(MemoryLayout<UInt16>.stride &- 1)
  let end = unsafe ptr + count

  
  while unsafe ptr < end128 {
    let pair = unsafe ptr.loadUnaligned(as: (UInt64, UInt64).self)
    let result = (pair.0 | pair.1) & asciiMask64
    guard result == 0 else { return false }
    unsafe ptr = unsafe ptr + MemoryLayout<(UInt64, UInt64)>.stride
  }
  
  // If we had enough bytes for two iterations of this, we would have hit
  // the loop above, so we only need to do this once
  if unsafe ptr < end64 {
    let value = unsafe ptr.loadUnaligned(as: UInt64.self)
    guard value & asciiMask64 == 0 else { return false }
    unsafe ptr = unsafe ptr + MemoryLayout<UInt64>.stride
  }
  
  if unsafe ptr < end32 {
    let value = unsafe ptr.loadUnaligned(as: UInt32.self)
    guard value & asciiMask32 == 0 else { return false }
    unsafe ptr = unsafe ptr + MemoryLayout<UInt32>.stride
  }
  
  if unsafe ptr < end16 {
    let value = unsafe ptr.loadUnaligned(as: UInt16.self)
    guard value & asciiMask16 == 0 else { return false }
    unsafe ptr = unsafe ptr + MemoryLayout<UInt16>.stride
  }

  if unsafe ptr < end {
    let value = unsafe ptr.loadUnaligned(fromByteOffset: 0, as: UInt8.self)
    guard value & asciiMask8 == 0 else { return false }
  }
  unsafe _internalInvariant(ptr == end || ptr + 1 == end)
  return true
}

extension String {

  internal static func _uncheckedFromASCII(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    if let smol = unsafe _SmallString(input) {
      return String(_StringGuts(smol))
    }

    let storage = unsafe __StringStorage.create(initializingFrom: input, isASCII: true)
    return storage.asString
  }

  @usableFromInline
  internal static func _fromASCII(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    unsafe _internalInvariant(_allASCII(input), "not actually ASCII")
    return unsafe _uncheckedFromASCII(input)
  }

  internal static func _fromASCIIValidating(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String? {
    if unsafe _fastPath(_allASCII(input)) {
      return unsafe _uncheckedFromASCII(input)
    }
    return nil
  }

  public // SPI(Foundation)
  static func _tryFromUTF8(_ input: UnsafeBufferPointer<UInt8>) -> String? {
    guard case .success(let extraInfo) = unsafe validateUTF8(input) else {
      return nil
    }

    return unsafe String._uncheckedFromUTF8(input, isASCII: extraInfo.isASCII)
  }

  @usableFromInline
  internal static func _fromUTF8Repairing(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> (result: String, repairsMade: Bool) {
    switch unsafe validateUTF8(input) {
    case .success(let extraInfo):
      return unsafe (String._uncheckedFromUTF8(
        input, asciiPreScanResult: extraInfo.isASCII
      ), false)
    case .error(_, let initialRange):
        return unsafe (repairUTF8(input, firstKnownBrokenRange: initialRange), true)
    }
  }

  internal static func _fromLargeUTF8Repairing(
    uninitializedCapacity capacity: Int,
    initializingWith initializer: (
      _ buffer: UnsafeMutableBufferPointer<UInt8>
    ) throws -> Int
  ) rethrows -> String {
    let result = try unsafe __StringStorage.create(
      uninitializedCodeUnitCapacity: capacity,
      initializingUncheckedUTF8With: initializer)

    switch unsafe validateUTF8(result.codeUnits) {
    case .success(let info):
      result._updateCountAndFlags(
        newCount: result.count,
        newIsASCII: info.isASCII
      )
      return result.asString
    case .error(_, let initialRange):
      defer { _fixLifetime(result) }
      //This could be optimized to use excess tail capacity
      return unsafe repairUTF8(result.codeUnits, firstKnownBrokenRange: initialRange)
    }
  }

  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    return unsafe _uncheckedFromUTF8(input, isASCII: _allASCII(input))
  }

  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>,
    isASCII: Bool
  ) -> String {
    if let smol = unsafe _SmallString(input) {
      return String(_StringGuts(smol))
    }

    let storage = unsafe __StringStorage.create(
      initializingFrom: input, isASCII: isASCII)
    return storage.asString
  }

  // If we've already pre-scanned for ASCII, just supply the result
  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>, asciiPreScanResult: Bool
  ) -> String {
    if let smol = unsafe _SmallString(input) {
      return String(_StringGuts(smol))
    }

    let isASCII = asciiPreScanResult
    let storage = unsafe __StringStorage.create(
      initializingFrom: input, isASCII: isASCII)
    return storage.asString
  }

  @usableFromInline
  internal static func _uncheckedFromUTF16(
    _ input: UnsafeBufferPointer<UInt16>
  ) -> String {
    // TODO(String Performance): Attempt to form smol strings

    // TODO(String performance): Skip intermediary array, transcode directly
    // into a StringStorage space.
    var contents: [UInt8] = []
    contents.reserveCapacity(input.count)
    let repaired = unsafe transcode(
      input.makeIterator(),
      from: UTF16.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { contents.append($0) })
    _internalInvariant(!repaired, "Error present")

    return unsafe contents.withUnsafeBufferPointer { unsafe String._uncheckedFromUTF8($0) }
  }

  @inline(never) // slow path
  private static func _slowFromCodeUnits<
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

    let str = unsafe contents.withUnsafeBufferPointer { unsafe String._uncheckedFromUTF8($0) }
    return (str, repaired)
  }

  @usableFromInline @inline(never) // can't be inlined w/out breaking ABI
  @_specialize(
    where Input == UnsafeBufferPointer<UInt8>, Encoding == Unicode.ASCII)
  @_specialize(
    where Input == Array<UInt8>, Encoding == Unicode.ASCII)
  internal static func _fromCodeUnits<
    Input: Collection,
    Encoding: Unicode.Encoding
  >(
    _ input: Input,
    encoding: Encoding.Type,
    repair: Bool
  ) -> (String, repairsMade: Bool)?
  where Input.Element == Encoding.CodeUnit {
    guard _fastPath(encoding == Unicode.ASCII.self) else {
      return _slowFromCodeUnits(input, encoding: encoding, repair: repair)
    }

    // Helper to simplify early returns
    func resultOrSlow(_ resultOpt: String?) -> (String, repairsMade: Bool)? {
      guard let result = resultOpt else {
        return _slowFromCodeUnits(input, encoding: encoding, repair: repair)
      }
      return (result, repairsMade: false)
    }

    #if !$Embedded
    // Fast path for untyped raw storage and known stdlib types
    if let contigBytes = input as? _HasContiguousBytes,
      contigBytes._providesContiguousBytesNoCopy {
      return resultOrSlow(contigBytes.withUnsafeBytes { rawBufPtr in
        let buffer = unsafe UnsafeBufferPointer(
          start: rawBufPtr.baseAddress?.assumingMemoryBound(to: UInt8.self),
          count: rawBufPtr.count)
        return unsafe String._fromASCIIValidating(buffer)
      })
    }
    #endif

    // Fast path for user-defined Collections
    if let strOpt = input.withContiguousStorageIfAvailable({
      (buffer: UnsafeBufferPointer<Input.Element>) -> String? in
      return unsafe String._fromASCIIValidating(
        UnsafeRawBufferPointer(buffer).bindMemory(to: UInt8.self))
    }) {
      return resultOrSlow(strOpt)
    }

    return unsafe resultOrSlow(Array(input).withUnsafeBufferPointer {
        let buffer = unsafe UnsafeRawBufferPointer($0).bindMemory(to: UInt8.self)
        return unsafe String._fromASCIIValidating(buffer)
      })
  }

  public // @testable
  static func _fromInvalidUTF16(
    _ utf16: UnsafeBufferPointer<UInt16>
  ) -> String {
    return unsafe String._fromCodeUnits(utf16, encoding: UTF16.self, repair: true)!.0
  }

  @usableFromInline
  internal static func _fromSubstring(
    _ substring: __shared Substring
  ) -> String {
    if substring._offsetRange == substring.base._offsetRange {
      return substring.base
    }

    return String._copying(substring)
  }

  @_alwaysEmitIntoClient
  @inline(never) // slow-path
  internal static func _copying(_ str: String) -> String {
    return String._copying(str[...])
  }
  @_alwaysEmitIntoClient
  @inline(never) // slow-path
  internal static func _copying(_ str: Substring) -> String {
    if _fastPath(str._wholeGuts.isFastUTF8) {
      var new = unsafe str._wholeGuts.withFastUTF8(range: str._offsetRange) {
        unsafe String._uncheckedFromUTF8($0)
      }
#if os(watchOS) && _pointerBitWidth(_32)
      if str._wholeGuts.isSmall,
         str._wholeGuts.count > _SmallString.contiguousCapacity() {
        new.reserveCapacity(_SmallString.capacity + 1)
        return new
      }
#endif
      return new
    }
    return unsafe Array(str.utf8).withUnsafeBufferPointer {
      unsafe String._uncheckedFromUTF8($0)
    }
  }

  @usableFromInline
  @available(SwiftStdlib 6.0, *)
  internal static func _validate<Encoding: Unicode.Encoding>(
    _ input: UnsafeBufferPointer<Encoding.CodeUnit>,
    as encoding: Encoding.Type
  ) -> String? {
    if encoding.CodeUnit.self == UInt8.self {
      let bytes = unsafe _identityCast(input, to: UnsafeBufferPointer<UInt8>.self)
      if encoding.self == UTF8.self {
        guard case .success(let info) = unsafe validateUTF8(bytes) else { return nil }
        return unsafe String._uncheckedFromUTF8(bytes, asciiPreScanResult: info.isASCII)
      } else if encoding.self == Unicode.ASCII.self {
        guard unsafe _allASCII(bytes) else { return nil }
        return unsafe String._uncheckedFromASCII(bytes)
      }
    }

    // slow-path
    var isASCII = true
    var buffer: UnsafeMutableBufferPointer<UInt8>
    unsafe buffer = UnsafeMutableBufferPointer.allocate(capacity: input.count*3)
    var written = buffer.startIndex

    var parser = Encoding.ForwardParser()
    var input = unsafe input.makeIterator()

    transcodingLoop:
    while true {
      switch unsafe parser.parseScalar(from: &input) {
      case .valid(let s):
        let scalar = Encoding.decode(s)
        guard let utf8 = Unicode.UTF8.encode(scalar) else {
          // transcoding error: clean up and return nil
          fallthrough
        }
        if buffer.count < written + utf8.count {
          let newCapacity = buffer.count + (buffer.count >> 1)
          let copy: UnsafeMutableBufferPointer<UInt8>
          unsafe copy = UnsafeMutableBufferPointer.allocate(capacity: newCapacity)
          let copied = unsafe copy.moveInitialize(
            fromContentsOf: buffer.prefix(upTo: written)
          )
          unsafe buffer.deallocate()
          unsafe buffer = unsafe copy
          written = copied
        }
        if isASCII && utf8.count > 1 {
          isASCII = false
        }
        written = unsafe buffer.suffix(from: written).initialize(fromContentsOf: utf8)
        break
      case .error:
        // validation error: clean up and return nil
        unsafe buffer.prefix(upTo: written).deinitialize()
        unsafe buffer.deallocate()
        return nil
      case .emptyInput:
        break transcodingLoop
      }
    }

    let storage = unsafe buffer.baseAddress.map {
      unsafe __SharedStringStorage(
        _mortal: $0,
        countAndFlags: _StringObject.CountAndFlags(
          count: buffer.startIndex.distance(to: written),
          isASCII: isASCII,
          isNFC: isASCII,
          isNativelyStored: false,
          isTailAllocated: false
        )
      )
    }
    return storage?.asString
  }
}
