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

  @usableFromInline
  typealias Storage = UncheckedStringStorage<Element>

  @safe
  @usableFromInline
  var storage: Storage

  @inlinable
  public var count: Int { return storage.count }

  @usableFromInline
  internal init(_ storage: Storage) {
    self.storage = storage
  }

  /// Constructs an empty string
  @inlinable
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

/// Unpacks a `.small` string's packed byte tuple into `buffer`, filling in
/// `data.count` elements starting at `buffer[0]`.
///
/// `data.bytes` is a packed tuple of `UInt8`s with no alignment guarantee
/// wider than 1 byte, so elements must be read with an alignment-agnostic
/// load, not by rebinding to `Element`.
///
/// Factored out of `withCString`/`withCharacterData` (which are themselves
/// generic over an unconstrained result/failure type and so cannot be
/// fully `@_specialize`d) so that this loop -- the only part of those
/// functions whose cost actually depends on `Element`'s width -- can be.
@_specialize(where Element == UInt8)
@_specialize(where Element == CChar)
@_specialize(where Element == UInt16)
@usableFromInline
internal func _unpackSmallUncheckedString<Element: FixedWidthInteger>(
  _ data: SmallUncheckedStringStorage<Element>,
  into buffer: UnsafeMutableBufferPointer<Element>
) {
  withUnsafeBytes(of: data.bytes) { rawBuffer in
    for i in 0..<Int(data.count) {
      unsafe buffer[i] = rawBuffer.loadUnaligned(
        fromByteOffset: i * MemoryLayout<Element>.stride,
        as: Element.self)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a NUL-terminated sequence of `Element`s.
  @inlinable
  public func withCString<R, Failure>(
    _ body: (UnsafePointer<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    switch storage {
      case .empty:
        return try unsafe withUnsafePointer(to: Element(0)) { (nulptr) throws(Failure) in
          return try unsafe body(nulptr)
        }
      case .small(let data):
        return try unsafe withUnsafeTemporaryAllocation(
          of: Element.self,
          capacity: Int(data.count) + 1
        ) { (buffer) throws(Failure) -> R in
          unsafe _unpackSmallUncheckedString(data, into: buffer)
          unsafe buffer[Int(data.count)] = 0
          return try unsafe body(buffer.baseAddress!)
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

/// Derive a pointer argument from an `UncheckedString` value parameter.
///
/// This always produces a NUL-terminated buffer: reusing the string's
/// existing storage directly when it's already NUL-terminated (`.dynamic`
/// storage always is; `.immortal` storage sometimes is), and materializing a
/// fresh NUL-terminated buffer otherwise -- mirroring `withCString`'s own
/// switch over `storage` above.
@available(SwiftStdlib 9999, *)
@_transparent
public // COMPILER_INTRINSIC
func _convertConstUncheckedStringToPointerArgument<
  Element: FixedWidthInteger,
  ToPointer: _Pointer
>(_ str: UncheckedString<Element>) -> (_ConvertedObject?, ToPointer) {
  switch str.storage {
  case .dynamic(let data):
    // Already a heap-allocated, NUL-terminated buffer -- reuse it directly.
    return _convertConstArrayToPointerArgument(data.characters)
  case .immortal(let data) where data.flags.contains(.nulTerminated):
    // Static, immortal, already NUL-terminated -- no owner needed.
    return (nil, unsafe ToPointer(data.characters._rawValue))
  default:
    // Empty, small, or non-NUL-terminated immortal: materialize a fresh
    // NUL-terminated buffer, mirroring `withCString`'s slow path for these
    // same three cases.
    //
    // Bulk-copy via `withCharacterData`/`withUnsafeBufferPointer` rather
    // than `Array(str)`, which would iterate element-by-element through
    // `subscript(_:)`, paying repeated closure-dispatch and bounds-check
    // overhead in place of a single bulk copy.
    var chars = str.withCharacterData { data in
      data.withUnsafeBufferPointer { buffer in
        unsafe Array(buffer)
      }
    }
    chars.append(0)
    return _convertConstArrayToPointerArgument(chars)
  }
}

// For UInt8, also allow CChar
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt8 {
  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a NUL-terminated sequence of `Element`s.
  @inlinable
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
  @_specialize(where Element == UInt8)
  @_specialize(where Element == CChar)
  @_specialize(where Element == UInt16)
  @inlinable
  public init(cString: UnsafePointer<Element>) {
    let len = unsafe fast_strlen(cString)
    let newStorage: Storage

    if len == 0 {
      newStorage = .empty
    } else if len <= SmallUncheckedStringStorage<Element>.capacity {
      newStorage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: cString, count: len)
        )
      )
    } else {
      newStorage = .dynamic(
        DynamicUncheckedStringStorage(
          characters: unsafe Array<Element>(
            unsafe UnsafeBufferPointer(start: cString,
                                       count: len + 1)
          ),
          flags: [.nulTerminated]
        )
      )
    }
    self.init(newStorage)
  }
}

// Support CChar as well where Element is UInt8
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt8 {
  /// Creates a string from a NUL-terminated sequence of characters.
  @inlinable
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
  @_specialize(where Element == UInt8)
  @_specialize(where Element == CChar)
  @_specialize(where Element == UInt16)
  @inlinable
  public init(immortalString: UnsafePointer<Element>) {
    let len = unsafe fast_strlen(immortalString)
    let newStorage: Storage

    if len == 0 {
      newStorage = .empty
    } else if len <= SmallUncheckedStringStorage<Element>.capacity {
      newStorage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: immortalString, count: len)
        )
      )
    } else {
      newStorage = .immortal(
        unsafe ImmortalUncheckedStringStorage(
          characters: immortalString,
          count: UInt32(len),
          flags: [.nulTerminated]
        )
      )
    }
    self.init(newStorage)
  }

  // Creates a string from an immortal string that isn't NUL terminated.
  @_specialize(where Element == UInt8)
  @_specialize(where Element == CChar)
  @_specialize(where Element == UInt16)
  @inlinable
  public init(immortalString: UnsafeBufferPointer<Element>) {
    let newStorage: Storage

    if immortalString.count == 0 {
      newStorage = .empty
    } else if immortalString.count <= SmallUncheckedStringStorage<Element>.capacity {
      newStorage = .small(unsafe SmallUncheckedStringStorage(immortalString))
    } else {
      newStorage = .immortal(
        unsafe ImmortalUncheckedStringStorage(
          characters: immortalString.baseAddress!,
          count: UInt32(immortalString.count),
          flags: []
        )
      )
    }
    self.init(newStorage)
  }
}

// For UInt16, provide dedicated overloads that call `fast_strlen` with a
// literally-typed `UnsafePointer<UInt16>` argument. The `Element`-generic
// initializers above can only ever bind their `fast_strlen(cString)` call to
// the generic `fast_strlen<T>` overload, since overload resolution runs at
// type-checking time, before `Element` is known to be concretely `UInt16` --
// `@_specialize`ing the generic initializer only clones that same generic
// loop, it can't retarget the call to a different overload. Duplicating the
// two bodies here is what actually lets `UncheckedString<UInt16>(cString:)`
// reach the faster, word-at-a-time `fast_strlen(_: UnsafePointer<UInt16>)`.
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt16 {
  /// Creates a string from a NUL-terminated sequence of characters.
  @inlinable
  public init(cString: UnsafePointer<UInt16>) {
    let len = unsafe fast_strlen(cString)
    let newStorage: Storage

    if len == 0 {
      newStorage = .empty
    } else if len <= SmallUncheckedStringStorage<UInt16>.capacity {
      newStorage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: cString, count: len)
        )
      )
    } else {
      newStorage = .dynamic(
        DynamicUncheckedStringStorage(
          characters: unsafe Array<UInt16>(
            unsafe UnsafeBufferPointer(start: cString,
                                       count: len + 1)
          ),
          flags: [.nulTerminated]
        )
      )
    }
    self.init(newStorage)
  }

  /// Creates a string from a NUL-terminated immortal string.
  @inlinable
  public init(immortalString: UnsafePointer<UInt16>) {
    let len = unsafe fast_strlen(immortalString)
    let newStorage: Storage

    if len == 0 {
      newStorage = .empty
    } else if len <= SmallUncheckedStringStorage<UInt16>.capacity {
      newStorage = .small(
        unsafe SmallUncheckedStringStorage(
          unsafe UnsafeBufferPointer(start: immortalString, count: len)
        )
      )
    } else {
      newStorage = .immortal(
        unsafe ImmortalUncheckedStringStorage(
          characters: immortalString,
          count: UInt32(len),
          flags: [.nulTerminated]
        )
      )
    }
    self.init(newStorage)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Calls the given closure with a buffer of `Element`s,
  /// which are *not* necessarily NUL-terminated.
  @inlinable
  public func withCharacterData<R, Failure>(
    _ body: (Span<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    switch storage {
      case .empty:
        return try body(Span<Element>())
      case .small(let data):
        return try unsafe withUnsafeTemporaryAllocation(
          of: Element.self,
          capacity: Int(data.count)
        ) { (buffer) throws(Failure) -> R in
          unsafe _unpackSmallUncheckedString(data, into: buffer)
          return try body(unsafe UnsafeBufferPointer(buffer).span)
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

// For UInt8, `.small` storage's packed byte tuple needs no unpacking: a
// `UInt8` has no alignment requirement beyond 1 byte, so the tuple's bytes
// already *are* a valid `Span<UInt8>` in place. This overrides the generic
// `withCharacterData` above (which must unpack `.small` storage into a
// temporary buffer for wider `Element`s, to satisfy their alignment) for
// this specific, and most common, instantiation.
@available(SwiftStdlib 9999, *)
extension UncheckedString where Element == UInt8 {
  /// Calls the given closure with a buffer of `Element`s,
  /// which are *not* necessarily NUL-terminated.
  @inlinable
  public func withCharacterData<R, Failure>(
    _ body: (Span<Element>) throws(Failure) -> R
  ) throws(Failure) -> R {
    switch storage {
      case .empty:
        return try body(Span<Element>())
      case .small(let data):
        return try withUnsafeBytes(of: data.bytes) { (rawBuffer) throws(Failure) -> R in
          let buffer = unsafe rawBuffer.bindMemory(to: UInt8.self)
          return try body(unsafe UnsafeBufferPointer(
            start: buffer.baseAddress, count: Int(data.count)
          ).span)
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

  @inlinable
  public var startIndex: Self.Index { 0 }
  @inlinable
  public var endIndex: Self.Index { count }

  @inlinable
  public func index(before i: Self.Index) -> Self.Index {
    return i - 1
  }
  @inlinable
  public func index(after i: Self.Index) -> Self.Index {
    return i + 1
  }

  @inlinable
  public subscript(_ ndx: Self.Index) -> Self.Element {
    precondition(ndx >= 0 && ndx < endIndex)
    // `.small` storage packs its elements into a `UInt8` tuple with no
    // alignment guarantee wider than 1 byte, so the element can be read
    // directly with an alignment-agnostic load instead of unpacking the
    // whole tuple into a temporary buffer via `withCharacterData` just to
    // extract one element.
    if case .small(let data) = storage {
      return withUnsafeBytes(of: data.bytes) { rawBuffer in
        unsafe rawBuffer.loadUnaligned(
          fromByteOffset: ndx * MemoryLayout<Element>.stride,
          as: Element.self)
      }
    }
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
  /// Provides bulk access to this string's contents as contiguous storage.
  ///
  /// Without this override, `Sequence`'s default implementation (which
  /// returns `nil` unconditionally) would force every generic algorithm
  /// that tries this fast path first (e.g. `Array(_:)`, `elementsEqual`)
  /// back onto element-by-element iteration via `subscript(_:)`, paying
  /// repeated closure-dispatch and bounds-check overhead in place of a
  /// single bulk copy.
  @inlinable
  @safe
  public func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try unsafe _uncheckedStringWithContiguousStorage(self, body)
  }

  /// Bulk-copies this string's elements into `buffer`.
  ///
  /// Without this override, `Sequence`'s default implementation copies one
  /// element at a time via `makeIterator()`/`subscript(_:)`, hitting the
  /// same per-element dispatch cost described on
  /// `withContiguousStorageIfAvailable` above. This is the hook that
  /// `Array(_:)`/`_copyCollectionToContiguousArray` actually calls.
  @inlinable
  public __consuming func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (IndexingIterator<Self>, UnsafeMutableBufferPointer<Element>.Index) {
    return unsafe _uncheckedStringCopyContents(self, initializing: buffer)
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
  @inlinable
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
  @usableFromInline
  var bounds: Range<Self.Index>

  @inlinable
  public var startIndex: Self.Index { return bounds.lowerBound }
  @inlinable
  public var endIndex: Self.Index { return bounds.upperBound }

  @inlinable
  public func index(before i: Self.Index) -> Self.Index {
    return i - 1
  }
  @inlinable
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

  @inlinable
  public subscript(_ ndx: Self.Index) -> Self.Element {
    precondition(bounds.contains(ndx))
    return base[ndx]
  }

  public subscript(bounds: Range<Self.Index>) -> UncheckedSubString<Element> {
    precondition(bounds.lowerBound >= startIndex && bounds.lowerBound < endIndex)
    precondition(bounds.upperBound >= startIndex && bounds.upperBound <= endIndex)
    return UncheckedSubString<Element>(base: base, bounds: bounds)
  }

  @inlinable
  @safe
  public func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return try unsafe _uncheckedStringWithContiguousStorage(self, body)
  }

  @inlinable
  public __consuming func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Element>
  ) -> (IndexingIterator<Self>, UnsafeMutableBufferPointer<Element>.Index) {
    return unsafe _uncheckedStringCopyContents(self, initializing: buffer)
  }

  @inlinable
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
  @inlinable
  public func isTriviallyIdentical(to other: Self) -> Bool {
    bounds == other.bounds && base.isTriviallyIdentical(to: other.base)
  }
}

// MARK: fast_strlen

@_specialize(where T == UInt8)
@_specialize(where T == CChar)
@usableFromInline
@inline(__always)
internal func fast_strlen<T: FixedWidthInteger>(_ str: UnsafePointer<T>) -> Int {
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
@usableFromInline
internal func fast_strlen(_ str: UnsafePointer<UInt16>) -> Int {
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

