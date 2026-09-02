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

@available(SwiftStdlib 9999, *)
extension UncheckedString: RangeReplaceableCollection {

  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= 0 && subrange.upperBound <= storage.count)

    let newCount = storage.count + newElements.count
    if newCount == 0 {
      storage = .empty
    } else if newCount <= SmallUncheckedStringStorage<Element>.capacity {
      switch storage {
        case .empty:
          storage = .small(SmallUncheckedStringStorage(newElements))
        case .small(_):
          var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
          chars.replaceSubrange(subrange, with: newElements)
          storage = .small(SmallUncheckedStringStorage(chars))
        default:
          fatalError("UncheckedString is unexpectedly not small when it should be")
      }
    } else {
      switch storage {
        case .empty:
          var chars = Array(newElements)
          chars.append(0)
          storage = .dynamic(
            DynamicUncheckedStringStorage(
              characters: chars,
              flags: [.nulTerminated]
            )
          )
        case .small(_), .immortal(_):
          var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
          chars.replaceSubrange(subrange, with: newElements)
          chars.append(0)
          storage = .dynamic(
            DynamicUncheckedStringStorage(
              characters: chars,
              flags: [.nulTerminated]
            )
          )
        case .dynamic(var rawStorage):
          rawStorage.characters.replaceSubrange(subrange, with: newElements)
          storage = .dynamic(rawStorage)
      }
    }
  }

  /// Adds a new element to the end of this string.
  ///
  /// The default `RangeReplaceableCollection.append(_:)` implementation
  /// routes through `insert(at:)` -> `replaceSubrange`, which re-derives
  /// the storage case and the new total count on every call. This
  /// dispatches on `storage` directly instead. In particular, `.dynamic`
  /// storage's backing array always keeps a trailing NUL as its last
  /// element (see `UncheckedStringStorage.count`), so appending in place
  /// means overwriting that NUL with the new element and appending a fresh
  /// one -- two O(1)-amortized `Array` operations with no element
  /// shifting, versus a full `replaceSubrange` call.
  public mutating func append(_ newElement: Element) {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters[rawStorage.characters.count - 1] = newElement
        rawStorage.characters.append(0)
        storage = .dynamic(rawStorage)
      case .empty:
        storage = .small(SmallUncheckedStringStorage(CollectionOfOne(newElement)))
      case .small(let data)
      where Int(data.count) < SmallUncheckedStringStorage<Element>.capacity:
        var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
        chars.append(newElement)
        storage = .small(SmallUncheckedStringStorage(chars))
      case .small(_), .immortal(_):
        var chars = withCharacterData { $0.withUnsafeBufferPointer { unsafe Array($0) } }
        chars.append(newElement)
        chars.append(0)
        storage = .dynamic(
          DynamicUncheckedStringStorage(characters: chars, flags: [.nulTerminated])
        )
    }
  }

  /// Adds the elements of a sequence to the end of this string.
  ///
  /// The default `RangeReplaceableCollection.append(contentsOf:)`
  /// implementation calls `append(_:)` once per element, paying the full
  /// storage-case dispatch for each individual element instead of once for
  /// the whole batch. For `.dynamic` storage -- the common case for a
  /// string that's being built up incrementally -- the trailing NUL is
  /// dropped, the whole sequence is appended in one bulk
  /// `Array.append(contentsOf:)` call (which already reserves capacity
  /// geometrically), and a fresh NUL is appended once at the end.
  public mutating func append<S: Sequence>(
    contentsOf newElements: __owned S
  ) where S.Element == Element {
    switch storage {
      case .dynamic(var rawStorage):
        storage = .empty
        rawStorage.characters.removeLast()
        rawStorage.characters.append(contentsOf: newElements)
        rawStorage.characters.append(0)
        storage = .dynamic(rawStorage)
      case .empty, .small(_), .immortal(_):
        replaceSubrange(endIndex..<endIndex, with: Array(newElements))
    }
  }

  /// Reserves enough space to store the specified number of elements.
  ///
  /// The default `RangeReplaceableCollection.reserveCapacity(_:)` is a
  /// no-op, which leaves no way to avoid repeated reallocation while
  /// building up a string one append at a time. `.dynamic` storage forwards
  /// directly to `Array.reserveCapacity`. `.empty`/`.small`/`.immortal`
  /// storage is promoted to `.dynamic` up front whenever the requested
  /// capacity wouldn't fit in `.small` storage anyway.
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
          DynamicUncheckedStringStorage(characters: chars, flags: [.nulTerminated])
        )
    }
  }

  /// Inserts a new element into this string at the specified position.
  ///
  /// The default `RangeReplaceableCollection.insert(_:at:)` always goes
  /// through `replaceSubrange`, even when `i == endIndex` and the call is
  /// really just an append. This fast-paths that common case to
  /// `append(_:)` instead.
  public mutating func insert(_ newElement: Element, at i: Index) {
    if i == endIndex {
      append(newElement)
    } else {
      replaceSubrange(i..<i, with: CollectionOfOne(newElement))
    }
  }

  /// Inserts the elements of a collection into this string at the
  /// specified position.
  ///
  /// Fast-paths `i == endIndex` to `append(contentsOf:)`, for the same
  /// reason as `insert(_:at:)` above.
  public mutating func insert<C: Collection>(
    contentsOf newElements: __owned C, at i: Index
  ) where C.Element == Element {
    if i == endIndex {
      append(contentsOf: newElements)
    } else {
      replaceSubrange(i..<i, with: newElements)
    }
  }

}

@available(SwiftStdlib 9999, *)
extension UncheckedString {
  /// Concatenates two `UncheckedString` values.
  ///
  /// `RangeReplaceableCollection` already provides a fully generic `+`
  /// (`Self, Other: RangeReplaceableCollection where Other.Element == Self.Element`),
  /// but its `Other` parameter is a bare generic parameter that doesn't name
  /// any concrete type. When an operand is a string literal, the constraint
  /// solver has nothing there to offer as a candidate binding beyond the
  /// literal's own default (`String`) -- which doesn't satisfy
  /// `Element == Element`, so the whole expression fails to type-check
  /// whenever both operands are literals, regardless of context.
  ///
  /// This concrete, non-generic overload gives the solver a directly
  /// nameable target type (`UncheckedString<Element>`) to try instead, which
  /// resolves the problem for the common case of concatenating two
  /// `UncheckedString` values (including literals).
  public static func + (
    lhs: UncheckedString<Element>,
    rhs: UncheckedString<Element>
  ) -> UncheckedString<Element> {
    var result = lhs
    result.append(contentsOf: rhs)
    return result
  }
}

@available(SwiftStdlib 9999, *)
extension UncheckedSubString: RangeReplaceableCollection {

  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= startIndex && subrange.lowerBound < endIndex)
    precondition(subrange.upperBound >= startIndex && subrange.upperBound <= endIndex)

    base.replaceSubrange(subrange, with: newElements)
  }

}
