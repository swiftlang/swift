//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

public
class _SwiftRawStringStorage : _SwiftNativeNSString {
  @nonobjc
  public // @testable
  final var capacity: Int

  @nonobjc
  public // @testable
  final var count: Int

  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Use the create method")
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal var rawStart: UnsafeMutableRawPointer {
    _abstract()
  }

  @_inlineable
  @nonobjc
  public // @testable
  final var unusedCapacity: Int {
    return capacity - count
  }
}

internal typealias _ASCIIStringStorage = _SwiftStringStorage<UInt8>
internal typealias _UTF16StringStorage = _SwiftStringStorage<UTF16.CodeUnit>

public final class _SwiftStringStorage<CodeUnit> : _SwiftRawStringStorage
where CodeUnit : UnsignedInteger & FixedWidthInteger {

  /// Create uninitialized storage of at least the specified capacity.
  @_inlineable
  @_versioned
  @nonobjc
  internal static func create(
    capacity: Int,
    count: Int = 0
  ) -> _SwiftStringStorage<CodeUnit> {
    _sanityCheck(count >= 0 && count <= capacity)
    let storage = Builtin.allocWithTailElems_1(
      _SwiftStringStorage<CodeUnit>.self,
      capacity._builtinWordValue, CodeUnit.self)

    let storageAddr = UnsafeMutableRawPointer(
      Builtin.bridgeToRawPointer(storage))
    let endAddr = (
      storageAddr + _swift_stdlib_malloc_size(storageAddr)
    ).assumingMemoryBound(to: CodeUnit.self)
    storage.capacity = endAddr - storage.start
    storage.count = count
    _sanityCheck(storage.capacity >= capacity)
    return storage
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal override final var rawStart: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(start)
  }

  // NSString API

  @_versioned
  @objc(initWithCoder:)
  convenience init(coder aDecoder: AnyObject) {
    _sanityCheckFailure("init(coder:) not implemented for _SwiftStringStorage")
  }

  @_versioned
  @objc(length)
  func length() -> UInt {
    return UInt(count)
  }

  @_versioned
  @objc(characterAtIndex:)
  func character(at index: Int) -> UInt16 {
    defer { _fixLifetime(self) }
    precondition(index >= 0 && index < count, "Index out of bounds")
    return UInt16(start[index])
  }

  @_versioned
  @objc(getCharacters:range:)
  func getCharacters(
    _ buffer: UnsafeMutablePointer<UInt16>,
    range aRange: _SwiftNSRange
  ) {
    _precondition(aRange.location >= 0 && aRange.length >= 0,
      "Range out of bounds")
    _precondition(aRange.location + aRange.length <= Int(count),
      "Range out of bounds")
    let slice = unmanagedView[
      aRange.location ..< aRange.location + aRange.length]
    slice._copy(
      into: UnsafeMutableBufferPointer<UTF16.CodeUnit>(
        start: buffer,
        count: aRange.length))
    _fixLifetime(self)
  }

  @_versioned
  @objc(_fastCharacterContents)
  func _fastCharacterContents() -> UnsafePointer<UInt16>? {
    guard CodeUnit.self == UInt16.self else { return nil }
    return UnsafePointer(rawStart.assumingMemoryBound(to: UInt16.self))
  }

  @_versioned // FIXME(sil-serialize-all)
  @objc(copyWithZone:)
  internal func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _SwiftStringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
}

extension _SwiftStringStorage {
  // Basic properties

  @_inlineable
  @_versioned
  @nonobjc
  internal final var start: UnsafeMutablePointer<CodeUnit> {
    return UnsafeMutablePointer(Builtin.projectTailElems(self, CodeUnit.self))
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final var end: UnsafeMutablePointer<CodeUnit> {
    return start + count
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final var capacityEnd: UnsafeMutablePointer<CodeUnit> {
    return start + capacity
  }

  @_inlineable
  @_versioned
  @nonobjc
  var usedBuffer: UnsafeMutableBufferPointer<CodeUnit> {
    return UnsafeMutableBufferPointer(start: start, count: count)
  }

  @_inlineable
  @_versioned
  @nonobjc
  var unusedBuffer: UnsafeMutableBufferPointer<CodeUnit> {
    return UnsafeMutableBufferPointer(start: end, count: capacity - count)
  }

  @_inlineable
  @_versioned
  @nonobjc
  var unmanagedView: _UnmanagedString<CodeUnit> {
    return _UnmanagedString(start: self.start, count: self.count)
  }
}

extension _SwiftStringStorage {
  // Append operations

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace<OtherCodeUnit>(
    _ other: _UnmanagedString<OtherCodeUnit>
  )
  where OtherCodeUnit : FixedWidthInteger & UnsignedInteger {
    let otherCount = Int(other.count)
    _sanityCheck(self.count + otherCount <= self.capacity)
    other._copy(into: self.unusedBuffer)
    self.count += otherCount
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace(_ other: _UnmanagedOpaqueString) {
    let otherCount = Int(other.count)
    _sanityCheck(self.count + otherCount <= self.capacity)
    other._copy(into: self.unusedBuffer)
    self.count += otherCount
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace<C: Collection>(contentsOf other: C)
  where C.Element == CodeUnit {
    let otherCount = Int(other.count)
    _sanityCheck(self.count + otherCount <= self.capacity)
    var (remainder, writtenUpTo) =
      other._copyContents(initializing: self.unusedBuffer)
    _precondition(remainder.next() == nil, "Collection underreported its count")
    _precondition(writtenUpTo == otherCount, "Collection misreported its count")
    count += otherCount
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlaceUTF16<C: Collection>(contentsOf other: C)
  where C.Element == UInt16 {
    let otherCount = Int(other.count)
    _sanityCheck(self.count + otherCount <= self.capacity)
    // TODO: Use _copyContents(initializing:) for UTF16->UTF16 case
    var it = other.makeIterator()
    for p in end ..< end + otherCount {
      p.pointee = CodeUnit(it.next()!)
    }
    _precondition(it.next() == nil, "Collection underreported its count")
    count += otherCount
  }
}

extension _SwiftStringStorage {
  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace(_ other: _StringGuts, range: Range<Int>) {
    if _fastPath(other.isASCII) {
      _appendInPlace(other._unmanagedASCIIView[range])
    } else if _fastPath(other._isContiguous) {
      _appendInPlace(other._unmanagedUTF16View[range])
    } else { // Opaque
      _appendInPlace(other._asOpaque()[range])
    }
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace(_ other: _StringGuts) {
    if _fastPath(other.isASCII) {
      _appendInPlace(other._unmanagedASCIIView)
    } else if _fastPath(other._isContiguous) {
      _appendInPlace(other._unmanagedUTF16View)
    } else { // Opaque
      _appendInPlace(other._asOpaque())
    }
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace(_ other: String) {
    self._appendInPlace(other._guts)
  }

  @_inlineable
  @_versioned
  @nonobjc
  internal final func _appendInPlace<S : StringProtocol>(_ other: S) {
    self._appendInPlace(
      other._wholeString._guts,
      range: other._encodedOffsetRange)
  }
}
