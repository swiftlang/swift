//===--- Utils.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Dictionary {
  @inline(__always)
  mutating func withValue<R>(
    for key: Key,
    default defaultValue: Value,
    body: (inout Value) throws -> R
  ) rethrows -> R {
    try body(&self[key, default: defaultValue])
  }

  mutating func insertValue(
    _ newValue: @autoclosure () -> Value,
    for key: Key
  ) -> Bool {
    if self[key] == nil {
      self[key] = newValue()
      return true
    }
    return false
  }
}
extension Sequence {
  func sorted<T: Comparable>(by keyPath: KeyPath<Element, T>) -> [Element] {
    sorted(by: { $0[keyPath: keyPath] < $1[keyPath: keyPath] })
  }
}

extension Collection where Element: Equatable {
  func commonPrefix(with other: some Collection<Element>) -> SubSequence {
    var (i, j) = (self.startIndex, other.startIndex)
    while i < self.endIndex, j < other.endIndex {
      guard self[i] == other[j] else { break }
      self.formIndex(after: &i)
      other.formIndex(after: &j)
    }
    return self[..<i]
  }
}

extension String {
  init(utf8 buffer: UnsafeRawBufferPointer) {
    guard !buffer.isEmpty else {
      self = ""
      return
    }
    self = String(
      unsafeUninitializedCapacity: buffer.count,
      initializingUTF8With: { dest in
        _ = dest.initialize(from: buffer)
        return buffer.count
      }
    )
  }

  init(utf8 buffer: UnsafeBufferPointer<UInt8>) {
    self.init(utf8: UnsafeRawBufferPointer(buffer))
  }

  init(utf8 slice: Slice<UnsafeRawBufferPointer>) {
    self = String(utf8: .init(rebasing: slice))
  }

  init(utf8 buffer: ByteScanner.Bytes) {
    self = buffer.withUnsafeBytes(String.init(utf8:))
  }

  func scanningUTF8<R>(_ scan: (inout ByteScanner) throws -> R) rethrows -> R {
    var tmp = self
    return try tmp.withUTF8 { utf8 in
      var scanner = ByteScanner(utf8)
      return try scan(&scanner)
    }
  }

  func tryDropPrefix(_ prefix: String) -> String? {
    guard hasPrefix(prefix) else { return nil }
    return String(dropFirst(prefix.count))
  }

  func escaped(addQuotesIfNeeded: Bool) -> String {
    scanningUTF8 { scanner in
      var needsQuotes = false
      let result = scanner.consumeWhole { consumer in
        switch consumer.peek {
        case "\\", "\"":
          consumer.append("\\")
        case " ", "$":  // $ is potentially a variable reference
          needsQuotes = true
        default:
          break
        }
      }
      let escaped = result.isUnchanged ? self : String(utf8: result)
      return addQuotesIfNeeded && needsQuotes ? "\"\(escaped)\"" : escaped
    }
  }

  var escaped: String {
    escaped(addQuotesIfNeeded: true)
  }

  init(_ str: StaticString) {
    self = str.withUTF8Buffer { utf8 in
      String(utf8: utf8)
    }
  }

  var isASCII: Bool {
    // Thanks, @testable interface!
    _classify()._isASCII
  }

  /// A more efficient version of replacingOccurrences(of:with:)/replacing(_:with:),
  /// since the former involves bridging, and the latter currently has no fast
  /// paths for strings.
  func replacing(_ other: String, with replacement: String) -> String {
    guard !other.isEmpty else {
      return self
    }
    guard isASCII else {
      // Not ASCII, fall back to slower method.
      return replacingOccurrences(of: other, with: replacement)
    }
    let otherUTF8 = other.utf8
    return scanningUTF8 { scanner in
      let bytes = scanner.consumeWhole { consumer in
        guard otherUTF8.count <= consumer.remaining.count else {
          // If there's no way we can eat the string, eat the remaining.
          consumer.eatRemaining()
          return
        }
        while consumer.trySkip(otherUTF8) {
          consumer.append(utf8: replacement)
        }
      }
      return bytes.isUnchanged ? self : String(utf8: bytes)
    }
  }
}

/// Pattern match by `is` property. E.g. `case \.isNewline: ...`
func ~= <T>(keyPath: KeyPath<T, Bool>, subject: T) -> Bool {
  return subject[keyPath: keyPath]
}

func ~= <T>(keyPath: KeyPath<T, Bool>, subject: T?) -> Bool {
  return subject?[keyPath: keyPath] == true
}
