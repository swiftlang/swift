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
@usableFromInline
struct UncheckedStringStorageFlags: OptionSet {
  @usableFromInline
  let rawValue: UInt8

  @usableFromInline
  init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  @usableFromInline
  static let nulTerminated = UncheckedStringStorageFlags(rawValue: 1 << 0)
}

// MARK: Storage Types

#if _pointerBitWidth(_64)

// On 64-bit platforms, UncheckedStringStorage is 16 bytes, and each of these
// structures must take up 15 bytes (because UncheckedStringStorage needs a
// discriminator byte).

@usableFromInline
struct SmallUncheckedStringStorage<CharType: FixedWidthInteger> {
  typealias Bytes = (
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8, UInt8, UInt8,
    UInt8, UInt8
  )

  var count: UInt8 = 0
  var bytes: Bytes = (0, 0, 0, 0,
                      0, 0, 0, 0,
                      0, 0, 0, 0,
                      0, 0)
}

@safe
@usableFromInline
struct ImmortalUncheckedStringStorage<CharType: FixedWidthInteger> {
  @usableFromInline
  var characters: UnsafePointer<CharType>
  @safe
  var count: UInt32
  @safe
  @usableFromInline
  var flags: UncheckedStringStorageFlags
  @safe
  var _reserved: (UInt8, UInt8) = (0, 0)
}

@usableFromInline
struct DynamicUncheckedStringStorage<CharType: FixedWidthInteger> {
  @usableFromInline
  var characters: [CharType]
  var _reserved: UInt32 = 0
  var flags: UncheckedStringStorageFlags
  var _reserved2: (UInt8, UInt8) = (0, 0)
}

#elseif _pointerBitWidth(_32)

// On 32-bit platforms, UncheckedStringStorage is 8 bytes, and each of these
// structures must take up 7 bytes (because UncheckedStringStorage needs a
// discriminator byte).

@usableFromInline
struct SmallUncheckedStringStorage<CharType: FixedWidthInteger> {
  typealias Bytes = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)

  var count: UInt8 = 0
  var bytes: Bytes = (0, 0, 0, 0, 0, 0)
}

@safe
@usableFromInline
struct ImmortalUncheckedStringStorage<CharType: FixedWidthInteger> {
  @usableFromInline
  var characters: UnsafePointer<CharType>
  @safe
  var count: UInt16
  @safe
  @usableFromInline
  var flags: UncheckedStringStorageFlags
}

@usableFromInline
struct DynamicUncheckedStringStorage<CharType: FixedWidthInteger> {
  @usableFromInline
  var characters: [CharType]
  var flags: UncheckedStringStorageFlags
  var _reserved: (UInt8, UInt8) = (0, 0)
}

#else

// We don't know the width of pointers on this platform(!)

#error("Unsupported pointer width")

#endif

extension SmallUncheckedStringStorage {
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

  static var capacity: Int {
    return MemoryLayout<Bytes>.size / MemoryLayout<CharType>.stride
  }
}

@usableFromInline
enum UncheckedStringStorage<CharType: FixedWidthInteger> {
  case empty
  case small(SmallUncheckedStringStorage<CharType>)
  case immortal(ImmortalUncheckedStringStorage<CharType>)
  case `dynamic`(DynamicUncheckedStringStorage<CharType>)

  @usableFromInline
  var count: Int {
    switch self {
      case .empty: return 0
      case .small(let rawStorage): return Int(rawStorage.count)
      case .immortal(let rawStorage): return Int(rawStorage.count)
      case .dynamic(let rawStorage): return Int(rawStorage.characters.count - 1)
    }
  }
}
