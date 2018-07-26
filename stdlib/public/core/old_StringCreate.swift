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

extension String {
  /// Constructs a `String` in `resultStorage` containing the given UTF-8.
  ///
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.
  @inlinable
  @_silgen_name("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    _ resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>,
    utf8CodeUnitCount: Int
  ) {
    resultStorage.initialize(to:
      String._fromWellFormedUTF8(
        UnsafeBufferPointer(start: start, count: utf8CodeUnitCount)))
  }

  @usableFromInline
  static func _fromUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool
  ) -> String? {
    if _isAllASCII(input) {
      return _fromASCII(input)
    }
    return _fromNonASCIIUTF8(input, repair: repair)
  }

  @usableFromInline
  static func _fromASCII(_ input: UnsafeBufferPointer<UInt8>) -> String {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }
    let storage = _SwiftStringStorage<UInt8>.create(
      capacity: input.count, count: input.count)
    _sanityCheck(storage.count == input.count)
    storage.start.initialize(
      from: input.baseAddress._unsafelyUnwrappedUnchecked, count: input.count)
    return String(_StringGuts(_large: storage))
  }

  @usableFromInline
  static func _fromWellFormedUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool = false
  ) -> String {
    return String._fromUTF8(input, repair: repair)!
  }

  @inlinable
  static func _fromWellFormedUTF16CodeUnits<C : RandomAccessCollection>(
    _ input: C, repair: Bool = false
  ) -> String where C.Element == UTF16.CodeUnit {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }
    return String._fromCodeUnits(
      input, encoding: UTF16.self, repairIllFormedSequences: repair)!
  }

  @inlinable
  internal static func _fromCodeUnits<
    Input: Collection, Encoding: Unicode.Encoding
  >(
    _ input: Input, encoding: Encoding.Type, repairIllFormedSequences: Bool
  ) -> String?
  where Input.Element == Encoding.CodeUnit {

    // TODO(SSO): small check

    // Determine how many UTF-16 code units we'll need
    let inputStream = input.makeIterator()
    guard let (utf16Count, isASCII) = UTF16.transcodedLength(
        of: inputStream,
        decodedAs: encoding,
        repairingIllFormedSequences: repairIllFormedSequences) else {
      return nil
    }

    let capacity = utf16Count
    if isASCII {
      if let small = _SmallUTF8String(
        _fromCodeUnits: input,
        utf16Length: utf16Count,
        isASCII: true,
        Encoding.self
      ) {
        return String(_StringGuts(small))
      }

      let storage = _SwiftStringStorage<UInt8>.create(
        capacity: capacity,
        count: utf16Count)
      var p = storage.start
      let sink: (UTF32.CodeUnit) -> Void = {
        p.pointee = UTF8.CodeUnit($0)
        p += 1
      }
      let hadError = transcode(
        input.makeIterator(),
        from: encoding, to: UTF32.self,
        stoppingOnError: true,
        into: sink)
      _sanityCheck(!hadError,
        "string cannot be ASCII if there were decoding errors")
      return String(_largeStorage: storage)
    } else {
      // TODO(SSO): Small transcoded string

      let storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
        capacity: capacity,
        count: utf16Count)
      var p = storage.start
      let sink: (UTF16.CodeUnit) -> Void = {
        p.pointee = $0
        p += 1
      }
      _ = transcode(
        input.makeIterator(),
        from: encoding, to: UTF16.self,
        stoppingOnError: !repairIllFormedSequences,
        into: sink)
      return String(_largeStorage: storage)
    }
  }

  internal static func _fromNonASCIIUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool
  ) -> String? {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }

    // Determine how many UTF-16 code units we'll need
    let inputStream = input.makeIterator()

    // TODO: Replace with much, much faster length check
    guard let (utf16Count, isASCII) = UTF16.transcodedLength(
        of: inputStream,
        decodedAs: UTF8.self,
        repairingIllFormedSequences: repair) else {
      return nil
    }

    let capacity = utf16Count
    _sanityCheck(!isASCII, "was given ASCII UTF-8")
    let storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
      capacity: capacity,
      count: utf16Count)
    var p = storage.start
    let sink: (UTF16.CodeUnit) -> Void = {
      p.pointee = $0
      p += 1
    }
    // TODO: Replace with much, much faster transcoding
    _ = transcode(
      input.makeIterator(),
      from: UTF8.self, to: UTF16.self,
      stoppingOnError: !repair,
      into: sink)
    return String(_largeStorage: storage)
  }

}