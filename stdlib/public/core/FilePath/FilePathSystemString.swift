/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

// MARK: - FilePath.CodeUnit helpers
//
// `FilePath.CodeUnit` is the storage element type used throughout this
// implementation: `CChar` on Unix, `UInt16` on Windows. The underscored
// extension members below are FilePath-internal helpers — they extend
// the underlying `CChar` / `UInt16` type with names a reader can
// recognise as path-byte constants.

@available(SwiftStdlib 9999, *)
extension FilePath.CodeUnit {
  internal static var _null: Self { 0 }
  internal static var _slash: Self { Self(_ascii: "/") }
  internal static var _backslash: Self { Self(_ascii: #"\"#) }
  internal static var _dot: Self { Self(_ascii: ".") }
  internal static var _colon: Self { Self(_ascii: ":") }
  internal static var _question: Self { Self(_ascii: "?") }
  internal static var _at: Self { Self(_ascii: "@") }

  internal init(_ascii s: Unicode.Scalar) {
    self = numericCast(UInt8(ascii: s))
  }

  /// Interpret this code unit as a drive-letter scalar, presented as
  /// written (no case normalization). On Windows, code units are
  /// UTF-16 and an unpaired surrogate yields `U+FFFD`.
  internal var _driveLetterScalar: Unicode.Scalar {
    #if os(Windows)
    return Unicode.Scalar(self) ?? "\u{FFFD}"
    #else
    return Unicode.Scalar(UInt8(bitPattern: self))
    #endif
  }
}

@available(SwiftStdlib 9999, *)
internal struct _SystemString: Sendable {
  internal typealias _Storage = [FilePath.CodeUnit]
  internal var nullTerminatedStorage: _Storage
}

@available(SwiftStdlib 9999, *)
extension _SystemString {
  internal init() {
    self.nullTerminatedStorage = [._null]
    _invariantCheck()
  }

  internal var length: Int {
    let len = nullTerminatedStorage.count - 1
    _internalInvariant(len == self.count)
    return len
  }

  internal init(nullTerminated storage: _Storage) {
    self.nullTerminatedStorage = storage
    _invariantCheck()
  }
}

@available(SwiftStdlib 9999, *)
extension _SystemString {
  fileprivate func _invariantsSatisfied() -> Bool {
    guard !nullTerminatedStorage.isEmpty else { return false }
    guard nullTerminatedStorage.last! == ._null else { return false }
    guard nullTerminatedStorage.firstIndex(of: ._null) == nullTerminatedStorage.count - 1 else {
      return false
    }
    return true
  }

  fileprivate func _invariantCheck() {
    #if DEBUG
    precondition(_invariantsSatisfied())
    #endif
  }
}

@available(SwiftStdlib 9999, *)
extension _SystemString: RandomAccessCollection, MutableCollection {
  internal typealias Element = FilePath.CodeUnit
  internal typealias Index = _Storage.Index
  internal typealias Indices = Range<Index>

  internal var startIndex: Index {
    nullTerminatedStorage.startIndex
  }

  internal var endIndex: Index {
    nullTerminatedStorage.index(before: nullTerminatedStorage.endIndex)
  }

  internal subscript(position: Index) -> FilePath.CodeUnit {
    _read {
      precondition(position >= startIndex && position <= endIndex)
      yield nullTerminatedStorage[position]
    }
    set(newValue) {
      precondition(position >= startIndex && position <= endIndex)
      nullTerminatedStorage[position] = newValue
      _invariantCheck()
    }
  }
}
@available(SwiftStdlib 9999, *)
extension _SystemString: RangeReplaceableCollection {
  internal mutating func replaceSubrange<C: Collection>(
    _ subrange: Range<Index>, with newElements: C
  ) where C.Element == FilePath.CodeUnit {
    defer { _invariantCheck() }
    nullTerminatedStorage.replaceSubrange(subrange, with: newElements)
  }

  internal mutating func reserveCapacity(_ n: Int) {
    defer { _invariantCheck() }
    nullTerminatedStorage.reserveCapacity(1 + n)
  }

  internal func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<FilePath.CodeUnit>) throws -> R
  ) rethrows -> R? {
    try unsafe nullTerminatedStorage.withContiguousStorageIfAvailable {
      try unsafe body(.init(start: $0.baseAddress, count: $0.count-1))
    }
  }

  internal mutating func withContiguousMutableStorageIfAvailable<R>(
    _ body: (inout UnsafeMutableBufferPointer<FilePath.CodeUnit>) throws -> R
  ) rethrows -> R? {
    defer { _invariantCheck() }
    return try unsafe nullTerminatedStorage.withContiguousMutableStorageIfAvailable {
      var buffer = unsafe UnsafeMutableBufferPointer<FilePath.CodeUnit>(
        start: $0.baseAddress, count: $0.count-1
      )
      return try unsafe body(&buffer)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension _SystemString: Hashable {}

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // _Storage backing — includes the trailing null byte.
  internal func withNullTerminatedCodeUnits<T>(
    _ f: (UnsafeBufferPointer<FilePath.CodeUnit>) throws -> T
  ) rethrows -> T {
    try unsafe nullTerminatedStorage.withUnsafeBufferPointer(f)
  }

  // Code units excluding the null terminator.
  internal func withCodeUnits<T>(
    _ f: (UnsafeBufferPointer<FilePath.CodeUnit>) throws -> T
  ) rethrows -> T {
    try unsafe withNullTerminatedCodeUnits {
      unsafe _internalInvariant($0.last == ._null)
      return try unsafe f(.init(start: $0.baseAddress, count: $0.count &- 1))
    }
  }
}

// MARK: - Span access

@available(SwiftStdlib 9999, *)
extension _SystemString {
  // The backing array INCLUDING the trailing null terminator. Per-subtype
  // spans extract their slice range from this base.
  internal var _nullTerminatedSpan: Span<FilePath.CodeUnit> {
    nullTerminatedStorage.span
  }

  // The backing array EXCLUDING the trailing null terminator.
  internal var _span: Span<FilePath.CodeUnit> {
    nullTerminatedStorage.span.extracting(0..<length)
  }
}

@available(SwiftStdlib 9999, *)
extension Slice<_SystemString> {
  internal func withCodeUnits<T>(
    _ f: (UnsafeBufferPointer<FilePath.CodeUnit>) throws -> T
  ) rethrows -> T {
    try unsafe base.nullTerminatedStorage.withUnsafeBufferPointer { fullBuf in
      let count = self.count
      _internalInvariant(startIndex >= 0 && startIndex + count <= fullBuf.count)
      let p = unsafe fullBuf.baseAddress.map { unsafe $0.advanced(by: startIndex) }
      return try unsafe f(UnsafeBufferPointer(start: p, count: count))
    }
  }
}

@available(SwiftStdlib 9999, *)
extension String {
  internal init?(validating str: _SystemString) {
    let decoded = str.string
    guard _SystemString(decoded) == str else { return nil }
    self = decoded
  }
}

@available(SwiftStdlib 9999, *)
extension _SystemString: ExpressibleByStringLiteral {
  internal init(stringLiteral: String) {
    self.init(stringLiteral)
  }

  internal init(_ string: String) {
    #if os(Windows)
    var chars = string.utf16.map { FilePath.CodeUnit($0) }
    #else
    var chars = string.utf8.map { FilePath.CodeUnit(bitPattern: $0) }
    #endif
    chars.append(._null)
    self.init(nullTerminated: chars)
  }
}

@available(SwiftStdlib 9999, *)
extension _SystemString: CustomStringConvertible, CustomDebugStringConvertible {
  internal var string: String {
    unsafe self.withCodeUnits { codeUnits in
      unsafe codeUnits.withMemoryRebound(to: FilePath._Encoding.CodeUnit.self) {
        unsafe String(decoding: $0, as: FilePath._Encoding.self)
      }
    }
  }

  internal var description: String { string }
  internal var debugDescription: String { description.debugDescription }
}
