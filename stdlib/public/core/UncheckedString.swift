//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A string value that is a collection of characters in an unspecified
/// encoding.
///
/// An `UncheckedString` is a series of characters, such as `"Swift"`, that
/// forms a collection.  Unlike `String`, `UncheckedString` is encoding
/// agnostic.  `UncheckedString` also does not bridge with Objective-C.
///
/// You can create new `UncheckedStrings` using string literals or string
/// interpolations, just as you can with `String`, however there are some
/// important differences, namely:
///
/// * `UncheckedString` supports an extra escape sequence, `\x{hh}`, which
///   allows you to specify raw code unit values in hexadecimal.  If you
///   use the `\u{hh}` escape sequence in an `UncheckedString`, it will
///   expand to the Unicode encoding corresponding to the string's element
///   size (i.e. UTF-8 for 8-bit, UTF-16 for 16-bit and UCS-4 for 32-bit).
///
/// * `UncheckedString` only allows interpolation of values that conform
///   to the `CustomUncheckedStringConvertible` protocol, rather than the
///   `CustomStringConvertible` protocol used by `String`.
///
/// * `UncheckedString` is generic over its element type, thus can be used
///   to hold both 8-bit and wide strings as required.
///
/// Swift will automatically create an `UncheckedString` if you specify a
/// string literal containing a `\x{hh}` escape and do not give any other
/// type information.
///
///     let name = "René Descartes"             // String
///     let iso8859Name = "Ren\x{e9} Descartes" // UncheckedString<UInt8>
///     let utf16Name: UncheckedString<UInt16> = "Ren\u{e9} Descartes"
///     let ucs4Name: UncheckedString<UInt32> = "Ren\u{39} Descartes"
///
///     // The following will generate a compile-time error:
///     let invalidName: String = "Ren\x{e9} Descartes"
///
/// Also unlike `String`, `UncheckedString` is generic over its element
/// type.
///
/// `UncheckedString` otherwise works in a similar manner to `String`.
///
/// Encoding and Decoding
/// =====================
///
/// One important use of `UncheckedString` is handling encoded character
/// data in forms other than UTF-8.
///
/// You can encode a Swift `String` using the `.encode()` method, for
/// instance:
///
///   // This is an `UncheckedString<UInt16>`:
///   let utf16Name = "Dagmar Karin Sørbøe".encode(as: UTF16.self)!
///
/// If you are encoding a string to an encoding that cannot represent the
/// characters in that string, you will get `nil`.  If you would prefer,
/// you can ask the `encode` method to substitute a character instead:
///
///   let asciiName = "Dagmar Karin Sørbøe".encode(as: ASCII.self,
///                             onUnsupportedEncoding: .substitute)
/// 
/// You can also decode an `UncheckedString` using the `.decode()` method:
///
///   let name = utf16Name.decode(as: UTF16.self)!
///
/// Or if you are uncertain a string will decode properly and want to use
/// a replacement character, you can do
///
///   let name = utf16Name.decode(as: UTF16.self,
///                               onInvalidEncoding: .substitute)
///
@available(SwiftStdlib 9999, *)
public struct UncheckedString<E: FixedWidthInteger>: UncheckedStringProtocol {
  public typealias Element = E

  typealias Storage = UncheckedStringStorage<Element>

  @safe
  var storage: Storage

  public var count: Int { return storage.count }

  internal init(_ storage: Storage) {
    self.storage = storage
  }

  /// Constructs an empty string
  public init() {
    self.init(UncheckedStringStorage.empty)
  }

  /// Constructs a string from a collection of character elements.
  ///
  /// - Parameters:
  ///
  ///   - c: A `Collection` containing the character elements.
  ///
  public init<C: Collection>(_ c: C) where C.Element == Element {
    if c.count == 0 {
      storage = .empty
    } else if c.count <= SmallUncheckedStringStorage<Element>.capacity {
      storage = .small(
        SmallUncheckedStringStorage(c)
      )
    } else {
      var chars = Array(c)
      chars.append(0)
      storage = .dynamic(
        DynamicUncheckedStringStorage(
          characters: chars,
          flags: [.nulTerminated]
        )
      )
    }
  }

  /// Constructs a string by adopting an Array.
  ///
  /// If the array is short, we may ignore it; otherwise we use it as the
  /// storage array for the `UncheckedString` directly.
  ///
  /// N.B. This initializer will append a `NUL` to the array if it keeps it.
  init(taking a: consuming Array<Element>) {
    if a.count == 0 {
      storage = .empty
      _ = consume a
    } else if a.count <= SmallUncheckedStringStorage<Element>.capacity {
      storage = .small(
        SmallUncheckedStringStorage(a)
      )
    } else {
      a.append(0)
      storage = .dynamic(
        DynamicUncheckedStringStorage(
          characters: a,
          flags: [.nulTerminated]
        )
      )
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a NUL-terminated sequence of `Element`s.
  public func withCString<R, Failure>(
    _ body: (UnsafePointer<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    switch storage {
      case .empty:
        return try unsafe withUnsafePointer(to: Element(0)) { (nulptr) throws(Failure) in
          return try unsafe body(nulptr)
        }
      case .small(let data):
        return try unsafe withUnsafeBytes(of: data.bytes) { (rawBuffer) throws(Failure) -> R in
          try unsafe withUnsafeTemporaryAllocation(
            of: Element.self,
            capacity: Int(data.count) + 1
          ) { (buffer) throws(Failure) -> R in
            // `rawBuffer` (a packed tuple of `UInt8`s) has no alignment
            // guarantee wider than 1 byte, so `Element`s must be read with
            // an alignment-agnostic load, not by rebinding to `Element`.
            for i in 0..<Int(data.count) {
              unsafe buffer[i] = rawBuffer.loadUnaligned(
                fromByteOffset: i * MemoryLayout<Element>.stride,
                as: Element.self)
            }
            unsafe buffer[Int(data.count)] = 0
            return try unsafe body(buffer.baseAddress!)
          }
        }
      case .immortal(let data):
        if !data.flags.contains(.nulTerminated) {
          let buffer = unsafe UnsafeMutableBufferPointer<Element>.allocate(
            capacity: Int(data.count) + 1
          )
          defer {
            unsafe buffer.deallocate()
          }
          let sourceBuffer = unsafe UnsafeBufferPointer(
            start: data.characters, count: Int(data.count)
          )
          let (_, ndx) = unsafe buffer.initialize(from: sourceBuffer)
          unsafe buffer[ndx] = 0
          return try unsafe body(buffer.baseAddress!)
        } else {
          return try unsafe body(data.characters)
        }
      case .dynamic(let data):
        // Dynamic strings are always NUL-terminated
        assert(data.flags.contains(.nulTerminated))
        return try unsafe data.characters.withUnsafeBufferPointer { (buffer) throws(Failure) -> R in
          return try unsafe body(buffer.baseAddress!)
        }
    }
  }
}

// For UInt8, also allow CChar
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt8 {
  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a NUL-terminated sequence of `Element`s.
  public func withCString<R, Failure>(
    _ body: (UnsafePointer<CChar>) throws(Failure) -> R
  ) throws(Failure) -> R {
    return try unsafe withCString {
      (ptr: UnsafePointer<UInt8>) throws(Failure) -> R in
      try unsafe body(UnsafeRawPointer(ptr).assumingMemoryBound(to: CChar.self))
    }
  }
}

// MARK: C string support

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Creates a string from a NUL-terminated sequence of characters.
  public init(cString: UnsafePointer<Element>) {
    let len = unsafe fast_strlen(cString)

    if len == 0 {
      storage = .empty
    } else if len <= SmallUncheckedStringStorage<Element>.capacity {
      storage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: cString, count: len)
        )
      )
    } else {
      storage = .dynamic(
        DynamicUncheckedStringStorage(
          characters: unsafe Array<Element>(
            unsafe UnsafeBufferPointer(start: cString,
                                       count: len + 1)
          ),
          flags: [.nulTerminated]
        )
      )
    }
  }
}

// Support CChar as well where Element is UInt8
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt8 {
  /// Creates a string from a NUL-terminated sequence of characters.
  public init(cString nullTerminatedCharacters: UnsafePointer<CChar>) {
    unsafe self.init(
      cString: unsafe UnsafeRawPointer(nullTerminatedCharacters).assumingMemoryBound(
        to: UInt8.self
      )
    )
  }
}

// MARK: Immortal string support

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Creates a string from a NUL-terminated immortal string.
  public init(immortalString: UnsafePointer<Element>) {
    let len = unsafe fast_strlen(immortalString)

    if len == 0 {
      storage = .empty
    } else if len <= SmallUncheckedStringStorage<Element>.capacity {
      storage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: immortalString, count: len)
        )
      )
    } else {
      storage = .immortal(
        unsafe ImmortalUncheckedStringStorage(
          characters: immortalString,
          count: UInt32(len),
          flags: [.nulTerminated]
        )
      )
    }
  }

  // Creates a string from an immortal string that isn't NUL terminated.
  public init(immortalString: UnsafeBufferPointer<Element>) {
    if immortalString.count == 0 {
      storage = .empty
    } else if immortalString.count <= SmallUncheckedStringStorage<Element>.capacity {
      storage = .small(unsafe SmallUncheckedStringStorage(immortalString))
    } else {
      storage = .immortal(
        unsafe ImmortalUncheckedStringStorage(
          characters: immortalString.baseAddress!,
          count: UInt32(immortalString.count),
          flags: []
        )
      )
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Calls the given closure with a buffer of `Element`s,
  /// which are *not* necessarily NUL-terminated.
  public func withCharacterData<R, Failure>(
    _ body: (Span<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    switch storage {
      case .empty:
        return try body(Span<Element>())
      case .small(let data):
        return try withUnsafeBytes(of: data.bytes) { (rawBuffer) throws(Failure) -> R in
          // `rawBuffer` (a packed tuple of `UInt8`s) has no alignment
          // guarantee wider than 1 byte, so `Element`s must be read with
          // an alignment-agnostic load, not by rebinding to `Element`.
          try unsafe withUnsafeTemporaryAllocation(
            of: Element.self,
            capacity: Int(data.count)
          ) { (buffer) throws(Failure) -> R in
            for i in 0..<Int(data.count) {
              unsafe buffer[i] = rawBuffer.loadUnaligned(
                fromByteOffset: i * MemoryLayout<Element>.stride,
                as: Element.self)
            }
            return try body(unsafe UnsafeBufferPointer(buffer).span)
          }
        }
      case .immortal(let data):
        return try body(unsafe UnsafeBufferPointer(start: data.characters,
                                            count: Int(data.count)).span)
      case .dynamic(let data):
        // Ignore the trailing NUL
        return try body(data.characters.span.extracting(droppingLast: 1))
    }
  }
}

// MARK: BidirectionalCollection

@available(SwiftStdlib 9999, *)
extension UncheckedString: BidirectionalCollection {
  public typealias SubSequence = UncheckedSubString<Element>
  public typealias Index = Int

  public var startIndex: Self.Index { 0 }
  public var endIndex: Self.Index { count }

  public func index(before i: Self.Index) -> Self.Index {
    return i - 1
  }
  public func index(after i: Self.Index) -> Self.Index {
    return i + 1
  }

  public subscript(_ ndx: Self.Index) -> Self.Element {
    precondition(ndx >= 0 && ndx < endIndex)
    return withCharacterData { data in
      return data[ndx]
    }
  }

  public subscript(bounds: Range<Self.Index>) -> UncheckedSubString<Element> {
    return UncheckedSubString<Element>(base: self, bounds: bounds)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Returns a Boolean value indicating whether this string is trivially
  /// identical to `other`.
  ///
  /// Comparing strings this way includes comparing (normally hidden)
  /// implementation details such as the memory location of any underlying
  /// storage. Therefore, identical strings are guaranteed to compare equal
  /// with `==`, but not all equal strings are considered identical.
  ///
  /// - Complexity: O(1)
  public func isTriviallyIdentical(to other: Self) -> Bool {
    switch (storage, other.storage) {
    case (.empty, .empty):
      return true
    case (.small(let lhs), .small(let rhs)):
      return lhs.count == rhs.count &&
        withUnsafeBytes(of: lhs.bytes) { lhsBytes in
          withUnsafeBytes(of: rhs.bytes) { rhsBytes in
            unsafe lhsBytes.elementsEqual(rhsBytes)
          }
        }
    case (.immortal(let lhs), .immortal(let rhs)):
      return unsafe lhs.characters == rhs.characters && lhs.count == rhs.count
    case (.dynamic(let lhs), .dynamic(let rhs)):
      return lhs.characters.isTriviallyIdentical(to: rhs.characters)
    default:
      return false
    }
  }
}

// MARK: UncheckedSubString

@available(SwiftStdlib 9999, *)
public struct UncheckedSubString<E: FixedWidthInteger>
  : UncheckedStringProtocol
{
  public typealias Element = E
  public typealias SubSequence = UncheckedSubString<Element>
  public typealias Index = Int

  public var base: UncheckedString<Element>
  var bounds: Range<Self.Index>

  public var startIndex: Self.Index { return bounds.lowerBound }
  public var endIndex: Self.Index { return bounds.upperBound }

  public func index(before i: Self.Index) -> Self.Index {
    return i - 1
  }
  public func index(after i: Self.Index) -> Self.Index {
    return i + 1
  }

  public init() {
    self.base = UncheckedString()
    self.bounds = 0..<0
  }

  init(base: UncheckedString<Element>, bounds: Range<Self.Index>) {
    self.base = base
    self.bounds = bounds
  }

  public subscript(_ ndx: Self.Index) -> Self.Element {
    precondition(bounds.contains(ndx))
    return base[ndx]
  }

  public subscript(bounds: Range<Self.Index>) -> UncheckedSubString<Element> {
    precondition(bounds.lowerBound >= startIndex && bounds.lowerBound < endIndex)
    precondition(bounds.upperBound >= startIndex && bounds.upperBound <= endIndex)
    return UncheckedSubString<Element>(base: base, bounds: bounds)
  }

  public func withCharacterData<R, Failure>(
    _ body: (Span<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    return try base.withCharacterData { (data) throws(Failure) -> R in
      return try body(data.extracting(bounds))
    }
  }

  /// Returns a Boolean value indicating whether this string is trivially
  /// identical to `other`.
  ///
  /// - Complexity: O(1)
  public func isTriviallyIdentical(to other: Self) -> Bool {
    bounds == other.bounds && base.isTriviallyIdentical(to: other.base)
  }
}

// MARK: fast_strlen

@inline(always)
fileprivate func fast_strlen<T: FixedWidthInteger>(_ str: UnsafePointer<T>) -> Int {
  // The compiler will optimize this to a call to C strlen() for UInt8
  var ptr = unsafe str
  while unsafe ptr.pointee != 0 {
    unsafe ptr += 1
  }
  return unsafe ptr - str
}

@inline(always)
fileprivate func containsNul16(_ word: UInt64) -> Bool {
  return ((word &- 0x0001000100010001) & ~word & 0x8000800080008000) != 0
}

// For UInt16, this is faster than the above
fileprivate func fast_strlen(_ str: UnsafePointer<UInt16>) -> Int {
  var ptr = unsafe str

  // Align
  var lowBits = Int(bitPattern: ptr) & 0x6
  if lowBits > 0 {
    if unsafe ptr.pointee == 0 {
      return unsafe ptr - str
    }
    unsafe ptr += 1
    lowBits -= 2
    if lowBits > 0 {
      if unsafe ptr.pointee == 0 {
        return unsafe ptr - str
      }
      unsafe ptr += 1
      lowBits -= 2
      if lowBits > 0 {
        if unsafe ptr.pointee == 0 {
          return unsafe ptr - str
        }
        unsafe ptr += 1
        lowBits -= 2
      }
    }
  }

  var ptr8 = unsafe UnsafeRawPointer(ptr).assumingMemoryBound(to: UInt64.self)
  while unsafe !containsNul16(ptr8.pointee) {
    unsafe ptr8 += 1
  }

  unsafe ptr = UnsafeRawPointer(ptr8).assumingMemoryBound(to: UInt16.self)

  if unsafe ptr.pointee != 0 {
    unsafe ptr += 1
    if unsafe ptr.pointee != 0 {
      unsafe ptr += 1
      if unsafe ptr.pointee != 0 {
        unsafe ptr += 1
      }
    }
  }

  return unsafe ptr - str
}

