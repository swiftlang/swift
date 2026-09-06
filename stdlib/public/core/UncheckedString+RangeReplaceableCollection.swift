//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// MARK: In-place small-storage mutation
//
// `SmallUncheckedStringStorage`'s packed byte tuple has no alignment
// guarantee wider than 1 byte (see `_unpackSmallUncheckedString` and
// `SmallUncheckedStringStorage.init<C: Collection>` in UncheckedString.swift/
// UncheckedStringStorage.swift, which already rely on this to justify using
// alignment-agnostic loads/stores instead of typed ones). Because
// `storeBytes(of:toByteOffset:as:)`/`loadUnaligned(fromByteOffset:as:)` are
// explicitly alignment-agnostic, and a raw-byte-count `copyMemory` doesn't
// care about element width either, mutating the tuple in place this way
// works for *any* `FixedWidthInteger` element, not just `UInt8` -- so
// `UncheckedString<UInt16>` gets the same allocation-free small-storage
// mutation as `UncheckedString<UInt8>`.
extension SmallUncheckedStringStorage {
  /// Appends `newElement` in place.
  ///
  /// - Parameter newElement: The element to append. The caller must ensure
  ///                          `count < Self.capacity`.
  @inline(__always)
  @usableFromInline
  #if $Embedded
  @_specialize(where CharType == UInt8)
  @_specialize(where CharType == UInt16)
  @_specialize(where CharType == CChar)
  #else
  @_specialize(exported: true, where CharType == UInt8)
  @_specialize(exported: true, where CharType == UInt16)
  @_specialize(exported: true, where CharType == CChar)
  #endif
  mutating func fastAppend(_ newElement: CharType) {
    let stride = MemoryLayout<CharType>.stride
    withUnsafeMutableBytes(of: &bytes) { buf in
      unsafe buf.storeBytes(
        of: newElement, toByteOffset: Int(count) * stride, as: CharType.self)
    }
    count += 1
  }

  /// Appends every element of `other` in place, as a single raw byte copy.
  ///
  /// - Parameter other: The elements to append. The caller must ensure
  ///                     `Int(count) + Int(other.count) <= Self.capacity`.
  @inline(__always)
  @usableFromInline
  #if $Embedded
  @_specialize(where CharType == UInt8)
  @_specialize(where CharType == UInt16)
  @_specialize(where CharType == CChar)
  #else
  @_specialize(exported: true, where CharType == UInt8)
  @_specialize(exported: true, where CharType == UInt16)
  @_specialize(exported: true, where CharType == CChar)
  #endif
  mutating func fastAppend(contentsOf other: Self) {
    let stride = MemoryLayout<CharType>.stride
    withUnsafeMutableBytes(of: &bytes) { buf in
      let base = unsafe buf.baseAddress!
      withUnsafeBytes(of: other.bytes) { otherBuf in
        unsafe (base + Int(count) * stride).copyMemory(
          from: otherBuf.baseAddress!, byteCount: Int(other.count) * stride)
      }
    }
    count += other.count
  }

  /// Inserts `newElement` at position `i` in place.
  ///
  /// - Parameters:
  ///   - newElement: The element to insert.
  ///   - i: The position to insert at. The caller must ensure
  ///        `count < Self.capacity` and `0 <= i && i <= count`.
  @inline(__always)
  @usableFromInline
  #if $Embedded
  @_specialize(where CharType == UInt8)
  @_specialize(where CharType == UInt16)
  @_specialize(where CharType == CChar)
  #else
  @_specialize(exported: true, where CharType == UInt8)
  @_specialize(exported: true, where CharType == UInt16)
  @_specialize(exported: true, where CharType == CChar)
  #endif
  mutating func fastInsert(_ newElement: CharType, at i: Int) {
    let stride = MemoryLayout<CharType>.stride
    withUnsafeMutableBytes(of: &bytes) { buf in
      let base = unsafe buf.baseAddress!
      let tailByteCount = (Int(count) - i) * stride
      if tailByteCount > 0 {
        unsafe (base + (i + 1) * stride).copyMemory(
          from: base + i * stride, byteCount: tailByteCount)
      }
      unsafe buf.storeBytes(of: newElement, toByteOffset: i * stride, as: CharType.self)
    }
    count += 1
  }

  /// Removes and returns the element at position `i` in place.
  ///
  /// - Parameter i: The position to remove. The caller must ensure
  ///                `0 <= i && i < count`.
  ///
  /// - Returns The removed element.
  @inline(__always)
  @usableFromInline
  @discardableResult
  #if $Embedded
  @_specialize(where CharType == UInt8)
  @_specialize(where CharType == UInt16)
  @_specialize(where CharType == CChar)
  #else
  @_specialize(exported: true, where CharType == UInt8)
  @_specialize(exported: true, where CharType == UInt16)
  @_specialize(exported: true, where CharType == CChar)
  #endif
  mutating func fastRemove(at i: Int) -> CharType {
    let stride = MemoryLayout<CharType>.stride
    let removed: CharType = withUnsafeMutableBytes(of: &bytes) { buf in
      let base = unsafe buf.baseAddress!
      let value = unsafe buf.loadUnaligned(fromByteOffset: i * stride, as: CharType.self)
      let tailByteCount = (Int(count) - i - 1) * stride
      if tailByteCount > 0 {
        unsafe (base + i * stride).copyMemory(
          from: base + (i + 1) * stride, byteCount: tailByteCount)
      }
      return value
    }
    count -= 1
    return removed
  }

  /// Replaces `subrange` with `newElements` in place.
  ///
  /// - Parameters:
  ///   - subrange: The range to replace. The caller must ensure the
  ///               resulting count (`Int(count) - subrange.count +
  ///               newElements.count`) does not exceed `Self.capacity`.
  ///   - newElements: The replacement elements.
  @inline(__always)
  @usableFromInline
  #if $Embedded
  @_specialize(where CharType == UInt8, C == UncheckedString<UInt8>)
  @_specialize(where CharType == UInt16, C == UncheckedString<UInt16>)
  @_specialize(where CharType == CChar, C == UncheckedString<CChar>)
  #else
  @_specialize(exported: true, where CharType == UInt8, C == UncheckedString<UInt8>)
  @_specialize(exported: true, where CharType == UInt16, C == UncheckedString<UInt16>)
  @_specialize(exported: true, where CharType == CChar, C == UncheckedString<CChar>)
  #endif
  mutating func fastReplaceSubrange<C: Collection>(
    _ subrange: Range<Int>, with newElements: C
  ) where C.Element == CharType {
    let stride = MemoryLayout<CharType>.stride
    let oldCount = Int(count)
    let newCount = newElements.count
    let finalCount = oldCount - subrange.count + newCount
    withUnsafeMutableBytes(of: &bytes) { buf in
      let base = unsafe buf.baseAddress!
      let tailByteCount = (oldCount - subrange.upperBound) * stride
      if tailByteCount > 0 {
        unsafe (base + (subrange.lowerBound + newCount) * stride).copyMemory(
          from: base + subrange.upperBound * stride, byteCount: tailByteCount)
      }
      var offset = subrange.lowerBound * stride
      for element in newElements {
        unsafe buf.storeBytes(of: element, toByteOffset: offset, as: CharType.self)
        offset += stride
      }
    }
    count = UInt8(finalCount)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedString: RangeReplaceableCollection {

  /// Replaces `subrange` with `newElements`.
  ///
  /// - Parameters:
  ///   - subrange: The range of elements to replace.
  ///   - newElements: The new elements to insert into the string.
  @inlinable
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= 0 && subrange.upperBound <= storage.count)

    if case .dynamic(var rawStorage) = storage {
      storage = .empty
      rawStorage.characters.replaceSubrange(subrange, with: newElements)
      rawStorage.count = UInt32(rawStorage.characters.count - 1)
      storage = .dynamic(rawStorage)
      return
    }

    // `storage` is never `.dynamic` from here on (handled and returned
    // above), so the branches below only need to distinguish `.empty`,
    // `.small`, and `.immortal`.
    let finalCount = storage.count - subrange.count + newElements.count

    if finalCount == 0 {
      storage = .empty
    } else if finalCount <= SmallUncheckedStringStorage<Element>.capacity {
      if case .empty = storage {
        // subrange is necessarily empty here.
        storage = .small(SmallUncheckedStringStorage(newElements))
      } else if case .small(var data) = storage {
        storage = .empty
        data.fastReplaceSubrange(subrange, with: newElements)
        storage = .small(data)
      } else {
        // .immortal
        var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
        chars.replaceSubrange(subrange, with: newElements)
        storage = .small(SmallUncheckedStringStorage(chars))
      }
    } else {
      if case .empty = storage {
        var chars = [Element]()
        chars.reserveCapacity(finalCount + 1)
        chars.append(contentsOf: newElements)
        chars.append(0)
        storage = .dynamic(
          DynamicUncheckedStringStorage(
            characters: chars,
            count: UInt32(chars.count - 1),
            flags: [.nulTerminated]
          )
        )
      } else {
        // .small or .immortal
        //
        // Reserve the final size up front, into a freshly-empty array,
        // rather than unpacking the small-storage bytes into their own
        // exactly-sized array first and reserving afterwards -- that would
        // allocate twice, since the reserve would immediately regrow and
        // copy past the exactly-sized first buffer.
        var chars = [Element]()
        chars.reserveCapacity(finalCount + 1)
        withCharacterData { data in
          data.withUnsafeBufferPointer { buf in
            unsafe chars.append(contentsOf: buf)
          }
        }
        chars.replaceSubrange(subrange, with: newElements)
        chars.append(0)
        storage = .dynamic(
          DynamicUncheckedStringStorage(
            characters: chars,
            count: UInt32(chars.count - 1),
            flags: [.nulTerminated]
          )
        )
      }
    }
  }

  /// Replaces `subrange` with the elements of another `UncheckedString`.
  ///
  /// - Parameters:
  ///   - subrange: The range of elements to replace.
  ///   - newElements: The new elements to insert into the string.
  @inlinable
  public mutating func replaceSubrange(
    _ subrange: Range<Self.Index>,
    with newElements: UncheckedString<Element>
  ) {
    newElements.withCharacterData { data in
      data.withUnsafeBufferPointer { buf in
        unsafe replaceSubrange(subrange, with: buf)
      }
    }
  }

  /// Adds a new element to the end of this string.
  ///
  /// - Parameter newElement: The element to append to the string.
  @inlinable
  public mutating func append(_ newElement: Element) {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters[rawStorage.characters.count - 1] = newElement
        rawStorage.characters.append(0)
        rawStorage.count += 1
        storage = .dynamic(rawStorage)
      case .empty:
        storage = .small(SmallUncheckedStringStorage(CollectionOfOne(newElement)))
      case .small(var data)
      where Int(data.count) < SmallUncheckedStringStorage<Element>.capacity:
        storage = .empty
        data.fastAppend(newElement)
        storage = .small(data)
      case .small(_), .immortal(_):
        var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
        chars.append(newElement)
        chars.append(0)
        storage = .dynamic(
          DynamicUncheckedStringStorage(
            characters: chars, count: UInt32(chars.count - 1), flags: [.nulTerminated])
        )
    }
  }

  /// Adds the elements of a sequence to the end of this string.
  ///
  /// - Parameter newElements: The elements to append to the string.
  @inlinable
  #if $Embedded
  @_specialize(where Element == UInt8, S == UncheckedString<UInt8>)
  @_specialize(where Element == UInt16, S == UncheckedString<UInt16>)
  @_specialize(where Element == CChar, S == UncheckedString<CChar>)
  #else
  @_specialize(exported: true, where Element == UInt8, S == UncheckedString<UInt8>)
  @_specialize(exported: true, where Element == UInt16, S == UncheckedString<UInt16>)
  @_specialize(exported: true, where Element == CChar, S == UncheckedString<CChar>)
  #endif
  public mutating func append<S: Sequence>(
    contentsOf newElements: __owned S
  ) where S.Element == Element {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters.removeLast()
        rawStorage.characters.append(contentsOf: newElements)
        rawStorage.characters.append(0)
        rawStorage.count = UInt32(rawStorage.characters.count - 1)
        storage = .dynamic(rawStorage)
      case .empty, .small(_), .immortal(_):
        replaceSubrange(endIndex..<endIndex, with: Array(newElements))
    }
  }

  /// Adds the elements of a collection to the end of this string.
  ///
  /// - Parameter newElements: The elements to append to the string.
  @inlinable
  #if $Embedded
  @_specialize(where Element == UInt8, C == UncheckedString<UInt8>)
  @_specialize(where Element == UInt16, C == UncheckedString<UInt16>)
  @_specialize(where Element == CChar, C == UncheckedString<CChar>)
  #else
  @_specialize(exported: true, where Element == UInt8, C == UncheckedString<UInt8>)
  @_specialize(exported: true, where Element == UInt16, C == UncheckedString<UInt16>)
  @_specialize(exported: true, where Element == CChar, C == UncheckedString<CChar>)
  #endif
  public mutating func append<C: Collection>(
    contentsOf newElements: __owned C
  ) where C.Element == Element {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters.removeLast()
        rawStorage.characters.append(contentsOf: newElements)
        rawStorage.characters.append(0)
        rawStorage.count = UInt32(rawStorage.characters.count - 1)
        storage = .dynamic(rawStorage)
      case .empty, .small(_), .immortal(_):
        replaceSubrange(endIndex..<endIndex, with: newElements)
    }
  }

  /// Adds the elements of another `UncheckedString` to the end of this one.
  ///
  /// - Parameter newElements: The string whose elements should be appended.
  @inlinable
  #if $Embedded
  @_specialize(where Element == UInt8)
  @_specialize(where Element == UInt16)
  @_specialize(where Element == CChar)
  #else
  @_specialize(exported: true, where Element == UInt8)
  @_specialize(exported: true, where Element == UInt16)
  @_specialize(exported: true, where Element == CChar)
  #endif
  public mutating func append(contentsOf newElements: UncheckedString<Element>) {
    if case .small(var data) = storage,
      case .small(let otherData) = newElements.storage,
      Int(data.count) + Int(otherData.count) <= SmallUncheckedStringStorage<Element>.capacity
    {
      storage = .empty
      data.fastAppend(contentsOf: otherData)
      storage = .small(data)
      return
    }
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters.removeLast()
        rawStorage.characters.append(contentsOf: newElements)
        rawStorage.characters.append(0)
        rawStorage.count = UInt32(rawStorage.characters.count - 1)
        storage = .dynamic(rawStorage)
      case .empty, .small(_), .immortal(_):
        replaceSubrange(endIndex..<endIndex, with: newElements)
    }
  }

  /// Reserves enough space to store the specified number of elements.
  ///
  /// - Parameter n: The minimum number of elements the string should be
  ///                able to store without reallocating.
  @inlinable
  public mutating func reserveCapacity(_ n: Int) {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        // `+ 1` for the trailing NUL that `.dynamic` storage always keeps.
        rawStorage.characters.reserveCapacity(n + 1)
        storage = .dynamic(rawStorage)
      case .empty, .small(_), .immortal(_):
        guard n > SmallUncheckedStringStorage<Element>.capacity else { return }
        var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
        chars.reserveCapacity(n + 1)
        chars.append(0)
        storage = .dynamic(
          DynamicUncheckedStringStorage(
            characters: chars, count: UInt32(chars.count - 1), flags: [.nulTerminated])
        )
    }
  }

  /// Inserts a new element into this string at the specified position.
  ///
  /// - Parameters:
  ///   - newElement: The new element to insert into the string.
  ///   - i: The position at which to insert the new element. `i` must be a
  ///        valid index into the string.
  @inlinable
  public mutating func insert(_ newElement: Element, at i: Index) {
    if i == endIndex {
      append(newElement)
    } else if case .small(var data) = storage,
      Int(data.count) < SmallUncheckedStringStorage<Element>.capacity
    {
      storage = .empty
      data.fastInsert(newElement, at: i)
      storage = .small(data)
    } else {
      replaceSubrange(i..<i, with: CollectionOfOne(newElement))
    }
  }

  /// Inserts the elements of a collection into this string at the
  /// specified position.
  ///
  /// - Parameters:
  ///   - newElements: The new elements to insert into the string.
  ///   - i: The position at which to insert the new elements. `i` must be a
  ///        valid index into the string.
  @inlinable
  public mutating func insert<C: Collection>(
    contentsOf newElements: __owned C, at i: Index
  ) where C.Element == Element {
    if i == endIndex {
      append(contentsOf: newElements)
    } else {
      replaceSubrange(i..<i, with: newElements)
    }
  }

  /// Removes and returns the element at the specified position.
  ///
  /// - Parameter i: The position of the element to remove. `i` must be a
  ///                valid index into the string, and not equal to the
  ///                string's end index.
  ///
  /// - Returns The removed element.
  @discardableResult
  @inlinable
  public mutating func remove(at i: Index) -> Element {
    if case .small(var data) = storage {
      storage = .empty
      let removed = data.fastRemove(at: i)
      storage = .small(data)
      return removed
    }
    let result = self[i]
    replaceSubrange(i..<index(after: i), with: EmptyCollection())
    return result
  }

  /// Removes all elements from this string.
  ///
  /// - Parameter keepCapacity: If `true` and the string's storage already
  ///                           has spare heap capacity, that capacity is
  ///                           retained for subsequent mutations; otherwise
  ///                           the string reverts to empty storage.
  @inlinable
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    switch storage {
      case .dynamic(var rawStorage) where keepCapacity:
        storage = .empty
        rawStorage.characters.removeAll(keepingCapacity: true)
        rawStorage.characters.append(0)
        rawStorage.count = 0
        storage = .dynamic(rawStorage)
      default:
        storage = .empty
    }
  }

}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Concatenates two `UncheckedString` values.
  ///
  /// - Parameters:
  ///   - lhs: The first string to concatenate.
  ///   - rhs: The second string to concatenate.
  ///
  /// - Returns The result of appending `rhs` to `lhs`.
  @inlinable
  public static func + (
    lhs: UncheckedString<Element>,
    rhs: UncheckedString<Element>
  ) -> UncheckedString<Element> {
    var result = lhs
    result.append(contentsOf: rhs)
    return result
  }

  /// Appends `rhs` to `lhs` in place.
  ///
  /// - Parameters:
  ///   - lhs: The string to append to.
  ///   - rhs: The string to append.
  @inlinable
  public static func += (
    lhs: inout UncheckedString<Element>,
    rhs: UncheckedString<Element>
  ) {
    lhs.append(contentsOf: rhs)
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedSubString: RangeReplaceableCollection {

  /// Replaces `subrange` with `newElements`.
  ///
  /// - Parameters:
  ///   - subrange: The range of elements to replace.
  ///   - newElements: The new elements to insert into the substring.
  @inlinable
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= startIndex && subrange.lowerBound < endIndex)
    precondition(subrange.upperBound >= startIndex && subrange.upperBound <= endIndex)

    _base.replaceSubrange(subrange, with: newElements)
  }

}
