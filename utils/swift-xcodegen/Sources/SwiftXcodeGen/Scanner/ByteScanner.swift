//===--- ByteScanner.swift ------------------------------------------------===//
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

/// Helper for eating bytes.
struct ByteScanner {
  typealias Cursor = UnsafeRawBufferPointer.Index

  private let input: UnsafeRawBufferPointer
  fileprivate(set) var cursor: Cursor

  init(_ input: UnsafeRawBufferPointer) {
    self.input = input
    self.cursor = input.startIndex
  }

  init(_ input: UnsafeBufferPointer<UInt8>) {
    self.init(UnsafeRawBufferPointer(input))
  }

  var hasInput: Bool { cursor != input.endIndex }
  var empty: Bool { !hasInput }

  var previous: Byte? {
    cursor > input.startIndex ? Byte(input[cursor - 1]) : nil
  }

  var peek: Byte? {
    hasInput ? Byte(input[cursor]) : nil
  }

  func peek(ahead n: Int) -> Byte? {
    precondition(n > 0)
    guard n < input.endIndex - cursor else { return nil }
    return Byte(input[cursor + n])
  }

  var whole: UnsafeRawBufferPointer {
    input
  }

  var remaining: UnsafeRawBufferPointer {
    .init(rebasing: input[cursor...])
  }

  func decodeUTF8<R: RangeExpression>(
    _ range: R
  ) -> String where R.Bound == Cursor {
    String(utf8: whole[range])
  }

  mutating func eat() -> Byte? {
    guard let byte = peek else { return nil }
    cursor += 1
    return byte
  }

  mutating func tryEat() -> Bool {
    eat() != nil
  }

  mutating func tryEat(where pred: (Byte) throws -> Bool) rethrows -> Bool {
    guard let c = peek, try pred(c) else { return false }
    cursor += 1
    return true
  }

  mutating func tryEat(_ byte: Byte) -> Bool {
    tryEat(where: { $0 == byte })
  }

  mutating func tryEat<S: Sequence>(_ seq: S) -> Bool where S.Element == UInt8 {
    let start = cursor
    for byte in seq {
      guard tryEat(Byte(byte)) else {
        cursor = start
        return false
      }
    }
    return true
  }

  mutating func tryEat(utf8 str: StaticString) -> Bool {
    str.withUTF8Buffer { utf8 in
      tryEat(utf8)
    }
  }

  // Prefer the StaticString overload where we can.
  @_disfavoredOverload
  mutating func tryEat(utf8 str: String) -> Bool {
    tryEat(str.utf8)
  }

  mutating func tryEating<T>(
    _ body: (inout ByteScanner) -> T?
  ) -> T? {
    var tmp = self
    guard let result = body(&tmp) else { return nil }
    self = tmp
    return result
  }

  mutating func skip(while pred: (Byte) throws -> Bool) rethrows {
    while try tryEat(where: pred) {}
  }

  mutating func skip(until pred: (Byte) throws -> Bool) rethrows {
    try skip(while: { try !pred($0) })
  }

  mutating func skip(untilAfter pred: (Byte) throws -> Bool) rethrows {
    if let char = peek, try !pred(char) {
      try skip(until: pred)
    }
    try skip(while: pred)
  }

  mutating func eat(
    while pred: (Byte) throws -> Bool
  ) rethrows -> UnsafeRawBufferPointer? {
    let start = cursor
    while try tryEat(where: pred) {}
    return start == cursor
      ? nil : UnsafeRawBufferPointer(rebasing: input[start ..< cursor])
  }

  /// Use a byte consumer to eat a series of bytes, returning `true` in `body`
  /// to continue consuming, `false` to end.
  mutating func consume(
    using body: (inout Consumer) throws -> Bool
  ) rethrows -> Bytes {
    var consumer = Consumer(self)
    while try body(&consumer) {}
    self = consumer.scanner
    return consumer.takeResult()
  }

  /// Similar to `consume(while:)`, but eats a character each time the body
  /// returns, and consumes the entire input.
  mutating func consumeWhole(
    _ body: (inout Consumer) throws -> Void
  ) rethrows -> Bytes {
    var consumer = Consumer(self, consumesWhole: true)
    repeat {
      guard consumer.hasInput else { break }
      try body(&consumer)
    } while consumer.eat()
    self = consumer.scanner
    return consumer.takeResult()
  }
}

extension ByteScanner {
  /// A wrapper type for a series of bytes consumed by Consumer, which uses
  /// a backing buffer to handle cases where intermediate bytes have been
  /// skipped. This must not outlive the underlying bytes being processed.
  struct Bytes {
    private enum Storage {
      case array
      case slice(Range<ByteScanner.Cursor>)
    }
    /// The array being stored when `storage == .array`. Defined out of line
    /// to avoid COW on mutation.
    private var _array: [UInt8] = []
    private var array: [UInt8] {
      _read {
        guard case .array = storage else {
          fatalError("Must be .array")
        }
        yield _array
      }
      _modify {
        guard case .array = storage else {
          fatalError("Must be .array")
        }
        yield &_array
      }
    }
    private var storage: Storage

    /// The underlying buffer being scanned.
    private let buffer: UnsafeRawBufferPointer

    /// The starting cursor.
    private let start: ByteScanner.Cursor

    /// The past-the-end position for the last cursor that was added.
    private var lastCursor: ByteScanner.Cursor

    /// If true, we're expecting to consume the entire buffer.
    private let consumesWhole: Bool

    fileprivate init(for scanner: ByteScanner, consumesWhole: Bool) {
      self.storage = .slice(scanner.cursor ..< scanner.cursor)
      self.buffer = scanner.whole
      self.start = scanner.cursor
      self.lastCursor = scanner.cursor
      self.consumesWhole = consumesWhole
    }
  }
}

extension ByteScanner.Bytes {
  fileprivate mutating func skip(_ range: Range<ByteScanner.Cursor>) {
    append(upTo: range.lowerBound)
    lastCursor = range.upperBound
    assert(lastCursor <= buffer.endIndex)
  }

  fileprivate mutating func skip(at cursor: ByteScanner.Cursor) {
    skip(cursor ..< cursor + 1)
  }

  @inline(__always)
  fileprivate mutating func switchToArray() {
    guard case .slice(let range) = storage else {
      fatalError("Must be a slice")
    }
    storage = .array
    if consumesWhole {
      array.reserveCapacity(buffer.count)
    }
    array += buffer[range]
  }

  fileprivate mutating func prepareToAppend(at cursor: ByteScanner.Cursor) {
    append(upTo: cursor)
    if case .slice = storage {
      // This must switch to owned storage, since it's not something present in
      // the underlying buffer.
      switchToArray()
    }
  }

  fileprivate mutating func append<S: Sequence>(
    contentsOf chars: S, at cursor: ByteScanner.Cursor
  ) where S.Element == UInt8 {
    prepareToAppend(at: cursor)
    array.append(contentsOf: chars)
  }

  fileprivate mutating func append(
    _ char: UInt8, at cursor: ByteScanner.Cursor
  ) {
    prepareToAppend(at: cursor)
    array.append(char)
  }

  fileprivate mutating func append(upTo cursor: ByteScanner.Cursor) {
    assert(cursor <= buffer.endIndex)
    guard cursor > lastCursor else { return }
    defer {
      lastCursor = cursor
    }
    switch storage {
    case .array:
      array += buffer[lastCursor ..< cursor]
    case .slice(var range):
      if range.isEmpty {
        // The slice is empty, we can move the start to the last cursor.
        range = lastCursor ..< lastCursor
      }
      if lastCursor == range.endIndex {
        // The slice is continuing from the last cursor, extend it.
        storage = .slice(range.startIndex ..< cursor)
      } else {
        // The last cursor is past the slice, we need to allocate.
        switchToArray()
        array += buffer[lastCursor ..< cursor]
      }
    }
  }

  var isEmpty: Bool {
    switch storage {
    case .array:
      array.isEmpty
    case .slice(let range):
      range.isEmpty
    }
  }

  /// Whether the set of bytes is known to be the same as the input. It is
  /// assumed that if a backing buffer were allocated, the result has changed.
  var isUnchanged: Bool {
    switch storage {
    case .array:
      false
    case .slice(let r):
      // Known to be the same if we're slicing the same input that we were
      // created with.
      r == start ..< buffer.endIndex
    }
  }

  var count: Int {
    switch storage {
    case .array:
      array.count
    case .slice(let range):
      range.count
    }
  }

  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    switch storage {
    case .array:
      try array.withUnsafeBytes(body)
    case .slice(let range):
      try body(.init(rebasing: buffer[range]))
    }
  }
}

extension ByteScanner {
  /// A simplified ByteScanner inferface that allows for efficient skipping
  /// while producing a continguous Bytes output. Additionally, it allows for
  /// injecting values into the output through calls to `append`.
  struct Consumer {
    fileprivate var scanner: ByteScanner
    private var result: ByteScanner.Bytes

    fileprivate init(_ scanner: ByteScanner, consumesWhole: Bool = false) {
      self.scanner = scanner
      self.result = .init(for: scanner, consumesWhole: consumesWhole)
    }

    var peek: Byte? {
      scanner.peek
    }

    func peek(ahead n: Int) -> Byte? {
      scanner.peek(ahead: n)
    }

    var hasInput: Bool {
      scanner.hasInput
    }

    var remaining: UnsafeRawBufferPointer {
      scanner.remaining
    }

    mutating func append(_ byte: Byte) {
      result.append(byte.rawValue, at: scanner.cursor)
    }

    mutating func append(utf8 str: String) {
      result.append(contentsOf: str.utf8, at: scanner.cursor)
    }

    mutating func takeResult() -> ByteScanner.Bytes {
      result.append(upTo: scanner.cursor)
      return result
    }

    mutating func eat() -> Bool {
      scanner.tryEat()
    }

    mutating func eatRemaining() {
      scanner.cursor = scanner.input.endIndex
    }

    mutating func skip() {
      result.skip(at: scanner.cursor)
      _ = scanner.eat()
    }

    private mutating func _skip(
      using body: (inout ByteScanner) throws -> Void
    ) rethrows {
      let start = scanner.cursor
      defer {
        if scanner.cursor != start {
          result.skip(start ..< scanner.cursor)
        }
      }
      try body(&scanner)
    }

    mutating func skip(while pred: (Byte) throws -> Bool) rethrows {
      try _skip(using: { try $0.skip(while: pred) })
    }

    mutating func skip(until pred: (Byte) throws -> Bool) rethrows {
      try _skip(using: { try $0.skip(until: pred) })
    }

    mutating func skip(untilAfter pred: (Byte) throws -> Bool) rethrows {
      try _skip(using: { try $0.skip(untilAfter: pred) })
    }

    mutating func trySkip<S: Sequence>(_ seq: S) -> Bool where S.Element == UInt8 {
      let start = scanner.cursor
      guard scanner.tryEat(seq) else { return false }
      result.skip(start ..< scanner.cursor)
      return true
    }

    mutating func trySkip(utf8 str: String) -> Bool {
      trySkip(str.utf8)
    }
  }
}
