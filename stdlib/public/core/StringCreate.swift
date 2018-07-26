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

extension String {
  @usableFromInline
  internal static func _fromASCII(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    if let smol = _SmallString(input) {
      return String(_StringGuts(smol))
    }

    // TODO(UTF8): Do we want to do remember ASCII-ness?
    let storage = _StringStorage.create(initializingFrom: input)
    return storage.asString
  }

  @usableFromInline
  internal static func _tryFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String? {
    // TODO(UTF8 perf): More efficient validation

    // TODO(UTF8 perf): Skip intermediary array
    var contents: [UInt8] = []
    contents.reserveCapacity(input.count)
    let repaired = transcode(
      input.makeIterator(),
      from: UTF8.self,
      to: UTF8.self,
      stoppingOnError: true,
      into: { contents.append($0) })
    guard !repaired else { return nil }

    return contents.withUnsafeBufferPointer { String._uncheckedFromUTF8($0) }
  }

  @usableFromInline
  internal static func _fromUTF8Repairing(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> (String, Bool) {
    // TODO(UTF8 perf): More efficient validation

    // TODO(UTF8 perf): Skip intermediary array
    var contents: [UInt8] = []
    contents.reserveCapacity(input.count)
    let repaired = transcode(
      input.makeIterator(),
      from: UTF8.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { contents.append($0) })
    let str = contents.withUnsafeBufferPointer { String._uncheckedFromUTF8($0) }
    return (str, repaired)
  }

  @usableFromInline
  internal static func _uncheckedFromUTF8(
    _ input: UnsafeBufferPointer<UInt8>
  ) -> String {
    if let smol = _SmallString(input) {
      return String(_StringGuts(smol))
    }

    // TODO(UTF8): Do we want to do an ascii scan?
    let storage = _StringStorage.create(initializingFrom: input)
    return storage.asString
  }

  @usableFromInline
  internal static func _uncheckedFromUTF16(
    _ input: UnsafeBufferPointer<UInt16>
  ) -> String {
    // TODO(UTF8): smol strings

    // TODO(UTF8): Faster transcoding...

    // TODO(UTF8): Skip intermediary array
    var contents: [UInt8] = []
    contents.reserveCapacity(input.count)
    let repaired = transcode(
      input.makeIterator(),
      from: UTF16.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { contents.append($0) })
    _sanityCheck(!repaired, "Error present")

    return contents.withUnsafeBufferPointer { String._uncheckedFromUTF8($0) }
  }

  internal func _withUnsafeBufferPointerToUTF8<R>(
    _ body: (UnsafeBufferPointer<UTF8.CodeUnit>) throws -> R
  ) rethrows -> R {
    if isEmpty {
      var nothing: UInt8 = 0
      return try body(UnsafeBufferPointer(start: &nothing, count: 0))
    }
    if _fastPath(_guts.isFastUTF8) {
      return try _guts.withFastUTF8(body)
    }

    unimplemented_utf8()
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
    // TODO(SSO): small check

    // TODO(UTF8): Skip intermediary array
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
}

