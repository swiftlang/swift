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

@_fixed_layout
@usableFromInline
class _SwiftRawStringStorage : __SwiftNativeNSString {
  @nonobjc
  @usableFromInline
  final var capacity: Int

  @nonobjc
  @usableFromInline
  final var count: Int

  @nonobjc
  internal init(_doNotCallMe: ()) {
    _sanityCheckFailure("Use the create method")
  }

  @inlinable
  @nonobjc
  internal var rawStart: UnsafeMutableRawPointer {
    _abstract()
  }

  @inlinable
  @nonobjc
  final var unusedCapacity: Int {
    _sanityCheck(capacity >= count)
    return capacity - count
  }
}

internal typealias _ASCIIStringStorage = _SwiftStringStorage<UInt8>
@usableFromInline // FIXME(sil-serialize-all)
internal typealias _UTF16StringStorage = _SwiftStringStorage<UTF16.CodeUnit>

@_fixed_layout
@usableFromInline
final class _SwiftStringStorage<CodeUnit>
  : _SwiftRawStringStorage, _NSStringCore
where CodeUnit : UnsignedInteger & FixedWidthInteger {

  /// Create uninitialized storage of at least the specified capacity.
  @usableFromInline
  @nonobjc
  @_specialize(where CodeUnit == UInt8)
  @_specialize(where CodeUnit == UInt16)
  internal static func create(
    capacity: Int,
    count: Int = 0
  ) -> _SwiftStringStorage<CodeUnit> {
    _sanityCheck(count >= 0 && count <= capacity)

#if arch(i386) || arch(arm)
#else
    // TODO(SR-7594): Restore below invariant
    // _sanityCheck(
    //   CodeUnit.self != UInt8.self || capacity > _SmallUTF8String.capacity,
    //   "Should prefer a small representation")
#endif // 64-bit

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

  @inlinable
  @nonobjc
  internal override final var rawStart: UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(start)
  }

#if _runtime(_ObjC)
  // NSString API

  @objc(initWithCoder:)
  @usableFromInline
  convenience init(coder aDecoder: AnyObject) {
    _sanityCheckFailure("init(coder:) not implemented for _SwiftStringStorage")
  }

  @objc(length)
  @usableFromInline
  var length: Int {
    return count
  }

  @objc(characterAtIndex:)
  @usableFromInline
  func character(at index: Int) -> UInt16 {
    defer { _fixLifetime(self) }
    precondition(index >= 0 && index < count, "Index out of bounds")
    return UInt16(start[index])
  }

  @objc(getCharacters:range:)
  @usableFromInline
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

  @objc(_fastCharacterContents)
  @usableFromInline
  func _fastCharacterContents() -> UnsafePointer<UInt16>? {
    guard CodeUnit.self == UInt16.self else { return nil }
    return UnsafePointer(rawStart.assumingMemoryBound(to: UInt16.self))
  }

  @objc(copyWithZone:)
  @usableFromInline
  func copy(with zone: _SwiftNSZone?) -> AnyObject {
    // While _SwiftStringStorage instances aren't immutable in general,
    // mutations may only occur when instances are uniquely referenced.
    // Therefore, it is safe to return self here; any outstanding Objective-C
    // reference will make the instance non-unique.
    return self
  }
#endif // _runtime(_ObjC)
}

extension _SwiftStringStorage {
  // Basic properties

  @inlinable
  @nonobjc
  internal final var start: UnsafeMutablePointer<CodeUnit> {
    return UnsafeMutablePointer(Builtin.projectTailElems(self, CodeUnit.self))
  }

  @inlinable
  @nonobjc
  internal final var end: UnsafeMutablePointer<CodeUnit> {
    return start + count
  }

  @inlinable
  @nonobjc
  internal final var capacityEnd: UnsafeMutablePointer<CodeUnit> {
    return start + capacity
  }

  @inlinable
  @nonobjc
  var usedBuffer: UnsafeMutableBufferPointer<CodeUnit> {
    return UnsafeMutableBufferPointer(start: start, count: count)
  }

  @inlinable
  @nonobjc
  var unusedBuffer: UnsafeMutableBufferPointer<CodeUnit> {
    @inline(__always)
    get {
      return UnsafeMutableBufferPointer(start: end, count: capacity - count)
    }
  }

  @inlinable
  @nonobjc
  var unmanagedView: _UnmanagedString<CodeUnit> {
    return _UnmanagedString(start: self.start, count: self.count)
  }
}

extension _SwiftStringStorage {
  // Append operations

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

  @nonobjc
  internal final func _appendInPlace(_ other: _UnmanagedOpaqueString) {
    let otherCount = Int(other.count)
    _sanityCheck(self.count + otherCount <= self.capacity)
    other._copy(into: self.unusedBuffer)
    self.count += otherCount
  }

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

  @_specialize(where C == Character._SmallUTF16, CodeUnit == UInt8)
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
  @nonobjc
  internal final func _appendInPlace(_ other: _StringGuts, range: Range<Int>) {
    if _slowPath(other._isOpaque) {
      _opaqueAppendInPlace(opaqueOther: other, range: range)
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      _appendInPlace(other._unmanagedASCIIView[range])
    } else {
      _appendInPlace(other._unmanagedUTF16View[range])
    }
  }

  @usableFromInline // @opaque
  internal final func _opaqueAppendInPlace(
    opaqueOther other: _StringGuts, range: Range<Int>
  ) {
    _sanityCheck(other._isOpaque)
    if other._isSmall {
      other._smallUTF8String[range].withUnmanagedUTF16 {
        self._appendInPlace($0)
      }
      return
    }
    defer { _fixLifetime(other) }
    _appendInPlace(other._asOpaque()[range])
  }

  @nonobjc
  internal final func _appendInPlace(_ other: _StringGuts) {
    if _slowPath(other._isOpaque) {
      _opaqueAppendInPlace(opaqueOther: other)
      return
    }

    defer { _fixLifetime(other) }
    if other.isASCII {
      _appendInPlace(other._unmanagedASCIIView)
    } else {
      _appendInPlace(other._unmanagedUTF16View)
    }
  }

  @usableFromInline // @opaque
  internal final func _opaqueAppendInPlace(opaqueOther other: _StringGuts) {
    _sanityCheck(other._isOpaque)
    if other._isSmall {
      other._smallUTF8String.withUnmanagedUTF16 {
        self._appendInPlace($0)
      }
      return
    }
    defer { _fixLifetime(other) }
    _appendInPlace(other._asOpaque())
  }

  @nonobjc
  internal final func _appendInPlace(_ other: String) {
    self._appendInPlace(other._guts)
  }

  @nonobjc
  internal final func _appendInPlace<S : StringProtocol>(_ other: S) {
    self._appendInPlace(
      other._wholeString._guts,
      range: other._encodedOffsetRange)
  }
}
