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

// MARK: Flags

/// Flags describing an `UncheckedString`'s underlying character data.
@frozen
@usableFromInline
struct UncheckedStringStorageFlags: OptionSet {
  /// The raw bitmask value of this set of flags.
  @usableFromInline
  let rawValue: UInt8

  /// Creates a flags value with the given raw bitmask.
  ///
  /// - Parameter rawValue: The raw bitmask to create the flags value from.
  @inlinable
  init(rawValue: UInt8) {
    self.rawValue = rawValue
  }
}

extension UncheckedStringStorageFlags {
  /// Indicates that a `0`-valued element is known to immediately follow
  /// the storage's character data in memory.
  @inlinable
  static var nulTerminated: UncheckedStringStorageFlags {
    UncheckedStringStorageFlags(rawValue: 1 << 0)
  }
}

// MARK: Storage Types

#if _pointerBitWidth(_64)

// On 64-bit platforms, UncheckedStringStorage is 16 bytes, and each of these
// structures must take up 15 bytes (because UncheckedStringStorage needs a
// discriminator byte).

/// Backing storage for an `UncheckedString` short enough to be stored
/// inline, without a separate heap allocation.
@frozen
@usableFromInline
struct SmallUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// The tuple type used to pack this storage's character data into
  /// `UncheckedStringStorage`'s available inline bytes.
  @usableFromInline
  typealias Bytes = (
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8
  )

  /// The number of elements stored inline in `bytes`.
  @usableFromInline
  var count: UInt8 = 0
  /// The packed byte representation of this storage's character data.
  @usableFromInline
  var bytes: Bytes = (0, 0, 0, 0,
                      0, 0, 0, 0,
                      0, 0, 0, 0,
                      0, 0)
}

/// Backing storage for an `UncheckedString` whose character data is
/// permanently alive (e.g. because it points into a string literal or
/// other static data), and so needs no reference counting or copying.
@safe
@frozen
@usableFromInline
struct ImmortalUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// A pointer to the permanently-alive character data.
  @usableFromInline
  var characters: UnsafePointer<CharType>
  /// The number of elements at `characters`.
  @safe
  @usableFromInline
  var count: UInt32
  /// Flags describing this storage's character data.
  @safe
  @usableFromInline
  var flags: UncheckedStringStorageFlags
  /// Reserved for future use.
  @safe
  @usableFromInline
  var _reserved: (UInt8, UInt8) = (0, 0)

  /// Creates storage over the given permanently-alive character data.
  ///
  /// - Parameters:
  ///   - characters: A pointer to the permanently-alive character data.
  ///   - count: The number of elements at `characters`.
  ///   - flags: Flags describing the character data.
  ///   - _reserved: Reserved for future use.
  @inlinable
  init(
    characters: UnsafePointer<CharType>,
    count: UInt32,
    flags: UncheckedStringStorageFlags,
    _reserved: (UInt8, UInt8) = (0, 0)
  ) {
    unsafe self.characters = characters
    self.count = count
    self.flags = flags
    self._reserved = _reserved
  }
}

/// Backing storage for an `UncheckedString` whose character data is held
/// in a separate, reference-counted, heap-allocated `Array`.
@frozen
@usableFromInline
struct DynamicUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// The heap-allocated character data, including a trailing NUL element.
  @usableFromInline
  var characters: [CharType]
  /// The logical element count, i.e. `characters.count - 1` (excluding the
  /// trailing NUL terminator `.dynamic` storage always keeps).
  @usableFromInline
  var count: UInt32
  /// Flags describing this storage's character data.
  @usableFromInline
  var flags: UncheckedStringStorageFlags
  /// Reserved for future use.
  @usableFromInline
  var _reserved2: (UInt8, UInt8) = (0, 0)

  /// Creates storage that adopts the given character data.
  ///
  /// - Parameters:
  ///   - characters: The heap-allocated character data, including a
  ///                 trailing NUL element.
  ///   - count: The logical element count, i.e. `characters.count - 1`.
  ///   - flags: Flags describing the character data.
  ///   - _reserved2: Reserved for future use.
  @inlinable
  init(
    characters: [CharType],
    count: UInt32,
    flags: UncheckedStringStorageFlags,
    _reserved2: (UInt8, UInt8) = (0, 0)
  ) {
    self.characters = characters
    self.count = count
    self.flags = flags
    self._reserved2 = _reserved2
  }
}

#elseif _pointerBitWidth(_32)

// On 32-bit platforms, UncheckedStringStorage is 8 bytes, and each of these
// structures must take up 7 bytes (because UncheckedStringStorage needs a
// discriminator byte).

/// Backing storage for an `UncheckedString` short enough to be stored
/// inline, without a separate heap allocation.
@frozen
@usableFromInline
struct SmallUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// The tuple type used to pack this storage's character data into
  /// `UncheckedStringStorage`'s available inline bytes.
  @usableFromInline
  typealias Bytes = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

  /// The number of elements stored inline in `bytes`.
  @usableFromInline
  var count: UInt8 = 0
  /// The packed byte representation of this storage's character data.
  @usableFromInline
  var bytes: Bytes = (0, 0, 0, 0, 0, 0)
}

/// Backing storage for an `UncheckedString` whose character data is
/// permanently alive (e.g. because it points into a string literal or
/// other static data), and so needs no reference counting or copying.
@safe
@frozen
@usableFromInline
struct ImmortalUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// A pointer to the permanently-alive character data.
  @usableFromInline
  var characters: UnsafePointer<CharType>
  /// The number of elements at `characters`.
  @safe
  @usableFromInline
  var count: UInt16
  /// Flags describing this storage's character data.
  @safe
  @usableFromInline
  var flags: UncheckedStringStorageFlags

  /// Creates storage over the given permanently-alive character data.
  ///
  /// - Parameters:
  ///   - characters: A pointer to the permanently-alive character data.
  ///   - count: The number of elements at `characters`.
  ///   - flags: Flags describing the character data.
  @inlinable
  init(
    characters: UnsafePointer<CharType>,
    count: UInt16,
    flags: UncheckedStringStorageFlags
  ) {
    unsafe self.characters = characters
    self.count = count
    self.flags = flags
  }
}

/// Backing storage for an `UncheckedString` whose character data is held
/// in a separate, reference-counted, heap-allocated `Array`.
@frozen
@usableFromInline
struct DynamicUncheckedStringStorage<CharType: FixedWidthInteger> {
  /// The heap-allocated character data, including a trailing NUL element.
  @usableFromInline
  var characters: [CharType]
  /// Flags describing this storage's character data.
  @usableFromInline
  var flags: UncheckedStringStorageFlags
  /// Reserved for future use.
  @usableFromInline
  var _reserved: (UInt8, UInt8) = (0, 0)

  /// The logical element count, computed as `characters.count - 1`.
  @inlinable
  var count: Int { characters.count - 1 }

  /// Creates storage that adopts the given character data.
  ///
  /// - Parameters:
  ///   - characters: The heap-allocated character data, including a
  ///                 trailing NUL element.
  ///   - count: The logical element count, i.e. `characters.count - 1`.
  ///            Only used to sanity-check the caller's arithmetic in debug
  ///            builds; the stored `count` is computed on demand instead.
  ///   - flags: Flags describing the character data.
  ///   - _reserved: Reserved for future use.
  @inlinable
  init(
    characters: [CharType],
    count: UInt32,
    flags: UncheckedStringStorageFlags,
    _reserved: (UInt8, UInt8) = (0, 0)
  ) {
    self.characters = characters
    self.flags = flags
    self._reserved = _reserved
    assert(Int(count) == characters.count - 1)
  }
}

#else

// We don't know the width of pointers on this platform(!)

#error("Unsupported pointer width")

#endif

// `ImmortalUncheckedStringStorage`'s `characters` pointer is safe to share
// across threads when `CharType` is `Sendable`: it points to
// permanently-alive data (e.g. a string literal) that this type never
// writes through -- `UncheckedString` mutation always promotes `.immortal`
// storage to `.dynamic` first instead of mutating in place. `@unchecked` is
// still required because a raw pointer is never automatically `Sendable`,
// regardless of its `Pointee`.
extension ImmortalUncheckedStringStorage: @unchecked Sendable
where CharType: Sendable {}

// `DynamicUncheckedStringStorage`'s `characters` array is `Sendable`
// whenever `Array<CharType>` is, i.e. whenever `CharType: Sendable`.
extension DynamicUncheckedStringStorage: Sendable where CharType: Sendable {}

extension SmallUncheckedStringStorage {
  /// Creates storage by packing the elements of `collection` into `bytes`.
  ///
  /// - Parameter collection: A `Collection` containing the character
  ///                          elements. The caller must ensure
  ///                          `collection.count <= Self.capacity`.
  @inlinable
  init<C: Collection>(_ collection: C) where C.Element == CharType {
    precondition(collection.count <= Self.capacity)
    count = UInt8(collection.count)
    // `bytes` is a packed tuple of `UInt8`s with no alignment guarantee
    // wider than 1 byte, so `CharType` elements must be written with an
    // alignment-agnostic store, not `initializeMemory(as:from:)` (which
    // requires the destination to already be properly aligned for
    // `CharType`).
    withUnsafeMutableBytes(of: &bytes) { rawBuffer in
      var offset = 0
      for element in collection {
        unsafe rawBuffer.storeBytes(of: element, toByteOffset: offset, as: CharType.self)
        offset += MemoryLayout<CharType>.stride
      }
    }
  }

  /// The maximum number of elements this storage kind can hold inline.
  @inlinable
  static var capacity: Int {
    return MemoryLayout<Bytes>.size / MemoryLayout<CharType>.stride
  }
}

/// The underlying representation of an `UncheckedString`'s contents,
/// chosen according to the string's size and lifetime.
@frozen
@usableFromInline
enum UncheckedStringStorage<CharType: FixedWidthInteger> {
  /// An empty string, with no character data at all.
  case empty
  /// A string short enough to be stored inline.
  case small(SmallUncheckedStringStorage<CharType>)
  /// A string whose character data is permanently alive.
  case immortal(ImmortalUncheckedStringStorage<CharType>)
  /// A string whose character data is held in a heap-allocated `Array`.
  case `dynamic`(DynamicUncheckedStringStorage<CharType>)

  /// The number of elements in the string.
  @inlinable
  var count: Int {
    switch self {
      case .empty: return 0
      case .small(let rawStorage): return Int(rawStorage.count)
      case .immortal(let rawStorage): return Int(rawStorage.count)
      case .dynamic(let rawStorage): return Int(rawStorage.count)
    }
  }
}

extension UncheckedStringStorage: Sendable where CharType: Sendable {}
