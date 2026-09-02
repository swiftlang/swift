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
  /// Appends `newElement` in place. The caller must ensure
  /// `count < Self.capacity`.
  @inline(__always)
  @usableFromInline
  mutating func fastAppend(_ newElement: CharType) {
    let stride = MemoryLayout<CharType>.stride
    withUnsafeMutableBytes(of: &bytes) { buf in
      unsafe buf.storeBytes(
        of: newElement, toByteOffset: Int(count) * stride, as: CharType.self)
    }
    count += 1
  }

  /// Appends every element of `other` in place, as a single raw byte copy.
  /// The caller must ensure `Int(count) + Int(other.count) <= Self.capacity`.
  ///
  /// Exists purely so that appending one `.small`-storage `UncheckedString`
  /// to another (the common case for `+=`/`+`) can copy bytes directly
  /// between the two packed tuples, instead of going through
  /// `fastReplaceSubrange`'s generic `for element in newElements` loop --
  /// which, for an `UncheckedString` source, means walking its `Collection`
  /// conformance one element at a time via `IndexingIterator`'s protocol
  /// witnesses and `UncheckedString`'s own `_read` subscript accessor.
  /// Sampling this under `sample`(1) showed that per-element path spending
  /// most of its time in a heap allocation/free pair for the accessor's
  /// coroutine context -- vastly more expensive than the 1-4 actual bytes
  /// being fetched. See `UncheckedString.append(contentsOf:
  /// UncheckedString<Element>)` below, the concrete overload that calls
  /// this.
  @inline(__always)
  @usableFromInline
  @_specialize(exported: true, where CharType == UInt8)
  @_specialize(exported: true, where CharType == UInt16)
  @_specialize(exported: true, where CharType == CChar)
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

  /// Inserts `newElement` at position `i` in place. The caller must ensure
  /// `count < Self.capacity` and `0 <= i && i <= count`.
  @inline(__always)
  @usableFromInline
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

  /// Removes and returns the element at position `i` in place. The caller
  /// must ensure `0 <= i && i < count`.
  @inline(__always)
  @usableFromInline
  @discardableResult
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

  /// Replaces `subrange` with `newElements` in place. The caller must
  /// ensure the resulting count (`Int(count) - subrange.count +
  /// newElements.count`) does not exceed `Self.capacity`.
  ///
  /// `@inline(__always)` alone doesn't get this inlined into its callers
  /// here: they call it while still generic over `C`/`CharType` themselves
  /// (concrete types only become known further up the call chain, e.g. in
  /// `UncheckedString.append<C: Collection>(contentsOf:)`), and a generic
  /// function can't be inlined into a caller that hasn't itself been
  /// specialized to concrete types yet. The `@_specialize`s below force
  /// concrete `UInt8`/`UInt16`/`CChar` instantiations (each against the
  /// matching `UncheckedString<CharType>`) to exist ahead of time so those
  /// instantiations -- rather than a slow, witness-table-dispatched generic
  /// call -- are what callers like `append(contentsOf:)` actually end up
  /// calling for the common case.
  @inline(__always)
  @usableFromInline
  @_specialize(exported: true, where CharType == UInt8, C == UncheckedString<UInt8>)
  @_specialize(exported: true, where CharType == UInt16, C == UncheckedString<UInt16>)
  @_specialize(exported: true, where CharType == CChar, C == UncheckedString<CChar>)
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

// `RangeReplaceableCollection`'s own default implementations of `+=`, `+`,
// `append`, `insert`, `remove`, and `removeAll` are unconditionally
// available (no `@available` gate at all). Since this extension's
// overrides of them are gated at `SwiftStdlib 9999` (like everything else
// in this feature -- `UncheckedString` doesn't exist before then), the
// constraint solver treats these overrides as strictly *less* available
// than those defaults, and at any concretely-typed call site it
// disqualifies/deprioritizes the less-available choice regardless of
// specificity. Confirmed with `-Xfrontend -debug-constraints`, which shows
// e.g. `(skipping unavailable disjunction choice ... UncheckedString
// extension.removeAll ...)`, even though normal overload ranking
// (`CSRanking.cpp`) otherwise correctly prefers a concrete override over a
// protocol-extension default.
//
// Tried and reverted: gating this extension at `StdlibDeploymentTarget
// 9999` (or a real, finite `StdlibDeploymentTarget` version below it, e.g.
// `6.4`) instead of `SwiftStdlib 9999`, on the theory that
// `StdlibDeploymentTarget`'s "collapses to the current deployment target in
// development builds" behavior would remove the asymmetry without
// affecting release builds. `StdlibDeploymentTarget 9999` didn't change
// anything (`AddSwiftStdlib.cmake`'s macro generation explicitly excludes
// the literal version `9999` from ever being downgraded -- confirmed via
// the actual generated `-define-availability` response file). A real
// finite version (`6.4`, which *does* get downgraded, to this build's
// actual `STDLIB_DEPLOYMENT_VERSION` of macOS 13.0) does remove the
// asymmetry for this extension specifically, but can't be applied to
// `UncheckedString`/`UncheckedSubString`/`UncheckedStringProtocol`
// themselves (a member can't be gated less restrictively than its
// enclosing type) without breaking their use of `Span` (via
// `withCharacterData`), which has a real, undowngraded macOS 26.0
// requirement -- above this build's macOS 13.0 deployment floor. Left at
// `SwiftStdlib 9999` pending investigation of whether the constraint
// solver's "use of an unavailable declaration" scoring should itself treat
// `9999` as available in non-strict-availability (development) builds, the
// same way the hard-error diagnostic path already does.
@available(SwiftStdlib 9999, *)
extension UncheckedString: RangeReplaceableCollection {

  /// Replaces `subrange` with `newElements`.
  ///
  /// A `.dynamic` source keeps its storage kind unconditionally, regardless
  /// of the resulting size (including down to zero elements): calling
  /// `replaceSubrange` is itself evidence that the string is being actively
  /// mutated, and further mutations are likely, so giving up the
  /// underlying `Array`'s already-paid-for capacity -- whether by demoting
  /// to `.small` or all the way to `.empty` -- risks an unnecessary
  /// reallocation on the very next mutation. `.empty`/`.small`/`.immortal`
  /// sources have no such capacity to lose (their "capacity" is either
  /// nonexistent or, for `.small`, an intrinsic part of the value with no
  /// separate heap buffer to reallocate), so they're still chosen purely by
  /// the resulting size: `.empty` if empty, `.small` if it fits, otherwise
  /// promoted to `.dynamic`.
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
        // Reserve the final size up front, into a freshly-empty array
        // (rather than unpacking the small-storage bytes into their own
        // exactly-sized array first and reserving afterwards), so there's
        // only ever one allocation total: an exactly-sized initial unpack
        // followed by `reserveCapacity` to a *larger* size is still two
        // allocations (the unpack's own, immediately superseded by the
        // reserve's regrow-and-copy) -- confirmed by sampling this path,
        // which showed most of its time in `_consumeAndCreateNew`
        // releasing that discarded first buffer. Appending into a
        // pre-reserved empty array instead means the unpack itself never
        // allocates its own buffer.
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
  /// Concrete, non-generic overload of `replaceSubrange(_:with:)`, for the
  /// specific case of another `UncheckedString<Element>` -- what
  /// `append(contentsOf:)` above calls this with once a source no longer
  /// fits in `.small` storage. Overload resolution prefers this over the
  /// `C: Collection`-constrained version above for the same reason as the
  /// analogous `append(contentsOf:)` overloads: a concrete, non-generic
  /// parameter type outranks a generic one that merely happens to be
  /// satisfiable.
  ///
  /// Without this, the generic version's own `Array.replaceSubrange(_:
  /// with: newElements)` call has to fetch `newElements`'s bytes through
  /// `Array`'s fully generic `C: Collection` path, which reaches
  /// `newElements.withContiguousStorageIfAvailable` through a chain of
  /// dispatch thunks and protocol witnesses (confirmed by sampling this
  /// path, which showed a large fraction of its time in
  /// `__swift_instantiateCanonicalPrespecializedGenericMetadata` -- i.e.
  /// paying for fresh generic metadata instantiation on every call, not
  /// just dispatch). Unpacking `newElements` directly here, via a
  /// concretely-typed call to its own `withCharacterData`, and handing
  /// `Array` a plain `UnsafeBufferPointer<Element>` instead of the abstract
  /// `UncheckedString`, skips all of that: `Array.replaceSubrange(_:with:
  /// UnsafeBufferPointer<Element>)` is a far simpler generic instantiation
  /// with no witness-table indirection to reach the source's bytes.
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
  /// The default `RangeReplaceableCollection.append(_:)` implementation
  /// routes through `insert(at:)` -> `replaceSubrange`, which re-derives
  /// the storage case and the new total count on every call. This
  /// dispatches on `storage` directly instead. In particular, `.dynamic`
  /// storage's backing array always keeps a trailing NUL as its last
  /// element (see `UncheckedStringStorage.count`), so appending in place
  /// means overwriting that NUL with the new element and appending a fresh
  /// one -- two O(1)-amortized `Array` operations with no element
  /// shifting, versus a full `replaceSubrange` call. The `.small` case
  /// mutates the packed byte tuple directly (see `fastAppend` above)
  /// instead of round-tripping through a temporary `Array`.
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
  /// The default `RangeReplaceableCollection.append(contentsOf:)`
  /// implementation calls `append(_:)` once per element, paying the full
  /// storage-case dispatch for each individual element instead of once for
  /// the whole batch. For `.dynamic` storage -- the common case for a
  /// string that's being built up incrementally -- the trailing NUL is
  /// dropped, the whole sequence is appended in one bulk
  /// `Array.append(contentsOf:)` call (which already reserves capacity
  /// geometrically), and a fresh NUL is appended once at the end.
  @inlinable
  @_specialize(exported: true, where Element == UInt8, S == UncheckedString<UInt8>)
  @_specialize(exported: true, where Element == UInt16, S == UncheckedString<UInt16>)
  @_specialize(exported: true, where Element == CChar, S == UncheckedString<CChar>)
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
  /// Overload of `append(contentsOf:)` above, constrained to `Collection`
  /// instead of `Sequence`. `newElements` here already has a known count and
  /// can be iterated more than once, so the `.empty`/`.small`/`.immortal`
  /// branch can hand it straight to `replaceSubrange` (whose own `.small`
  /// fast path, `fastReplaceSubrange`, iterates any `Collection` in place)
  /// instead of first materializing it into a throwaway `Array` -- avoiding
  /// a heap allocation for the common case of appending one small
  /// `UncheckedString` to another. Overload resolution prefers this over
  /// the `Sequence`-constrained version above whenever the argument's
  /// static type is already known to conform to `Collection` (e.g. another
  /// `UncheckedString`, as `+=`/`+` below call this with), the same way
  /// `BidirectionalCollection.reversed()` is preferred over
  /// `Sequence.reversed()` for a `Collection`-typed argument.
  @inlinable
  @_specialize(exported: true, where Element == UInt8, C == UncheckedString<UInt8>)
  @_specialize(exported: true, where Element == UInt16, C == UncheckedString<UInt16>)
  @_specialize(exported: true, where Element == CChar, C == UncheckedString<CChar>)
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
  /// Concrete, non-generic overload of `append(contentsOf:)`, for the
  /// specific case of another `UncheckedString<Element>` -- what `+=`/`+`
  /// below actually call this with. Overload resolution prefers this over
  /// the `C: Collection`-constrained version above for the same reason that
  /// version is preferred over the `S: Sequence` one: a concrete,
  /// non-generic parameter type outranks a generic one that merely happens
  /// to be satisfiable. When both sides are `.small` and the combined
  /// length still fits, this reaches `fastAppend(contentsOf:)` (see above),
  /// which copies bytes directly between the two packed tuples with no
  /// per-element iteration at all -- the fastest path available, and the
  /// one that matters for the common case of building up a short string a
  /// few pieces at a time. Every other storage combination falls back to
  /// the same logic as the `Collection`-constrained overload above.
  @inlinable
  @_specialize(exported: true, where Element == UInt8)
  @_specialize(exported: true, where Element == UInt16)
  @_specialize(exported: true, where Element == CChar)
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
  /// The default `RangeReplaceableCollection.reserveCapacity(_:)` is a
  /// no-op, which leaves no way to avoid repeated reallocation while
  /// building up a string one append at a time. `.dynamic` storage forwards
  /// directly to `Array.reserveCapacity`. `.empty`/`.small`/`.immortal`
  /// storage is promoted to `.dynamic` up front whenever the requested
  /// capacity wouldn't fit in `.small` storage anyway.
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
  /// The default `RangeReplaceableCollection.insert(_:at:)` always goes
  /// through `replaceSubrange`, even when `i == endIndex` and the call is
  /// really just an append. This fast-paths that common case to
  /// `append(_:)` instead, and the `.small`, non-`endIndex` case mutates
  /// the packed byte tuple directly (see `fastInsert` above) instead of
  /// round-tripping through a temporary `Array`.
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
  /// Fast-paths `i == endIndex` to `append(contentsOf:)`, for the same
  /// reason as `insert(_:at:)` above.
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
  /// The default `RangeReplaceableCollection.remove(at:)` always goes
  /// through `replaceSubrange`. This fast-paths the `.small` case to mutate
  /// the packed byte tuple directly (see `fastRemove` above) instead of
  /// round-tripping through a temporary `Array`.
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
  /// When asked to keep capacity, goes through a `.dynamic` source directly:
  /// clearing the backing `Array` in place reuses its existing buffer with
  /// no reallocation, staying `.dynamic` (zero-length) rather than giving
  /// up that buffer -- same reasoning as `replaceSubrange` above: a string
  /// already being mutated via `removeAll` is likely to be mutated
  /// further. `keepCapacity == false`, by contrast, is the caller
  /// explicitly saying it does *not* need the capacity kept, so it drops
  /// straight to `.empty`, same as every other source (`.empty`/`.small`/
  /// `.immortal`, which have no heap buffer worth keeping regardless of
  /// `keepCapacity`).
  ///
  /// Caveat: at a concretely-typed call site, this override isn't actually
  /// reached at all -- see the comment above the `+`/`+=` operators below.
  /// `RangeReplaceableCollection`'s own default takes the exact same
  /// `keepCapacity == false` -> `self = Self()` shortcut in that case
  /// (never calling `replaceSubrange` either), so the observed behavior
  /// matches this override's regardless.
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

// `+`/`+=` are the operators that originally surfaced the issue described
// in the comment above `extension UncheckedString: RangeReplaceableCollection`
// further up in this file: `swift-frontend -emit-sil` on a call site with
// both operands statically typed as `UncheckedString<UInt8>` showed the
// call resolving to `RangeReplaceableCollection`'s generic `+=`
// (`function_ref static RangeReplaceableCollection.+= infix<A>(_:_:)`),
// never to the concrete overload below -- even though `Array`'s own
// `+=`/`+` overrides in Array.swift (which carry no availability
// annotation at all, since `Array` needs none) follow this exact same
// pattern successfully. `-Xfrontend -debug-constraints` traced this to the
// asymmetric availability itself: the constraint solver
// disqualifies/deprioritizes an `@available`-gated override in favor of an
// ungated protocol-extension default, regardless of specificity. See that
// same comment for why `StdlibDeploymentTarget` doesn't fix this for
// `UncheckedString` itself, and why this is left at `SwiftStdlib 9999` for
// now.
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
  /// `RangeReplaceableCollection` already provides a fully generic `+=`;
  /// this concrete, non-generic overload exists purely for performance
  /// (see the comment above this extension) so that a call with both
  /// operands statically typed as `UncheckedString<Element>` resolves
  /// directly to `append(contentsOf:)` instead of through the generic
  /// default's own indirection.
  @inlinable
  public static func += (
    lhs: inout UncheckedString<Element>,
    rhs: UncheckedString<Element>
  ) {
    lhs.append(contentsOf: rhs)
  }
}

// See the comment above `extension UncheckedString: RangeReplaceableCollection`
// further up in this file re: `SwiftStdlib 9999` vs `StdlibDeploymentTarget`.
@available(SwiftStdlib 9999, *)
extension UncheckedSubString: RangeReplaceableCollection {

  @inlinable
  public mutating func replaceSubrange<C>(
    _ subrange: Range<Self.Index>,
    with newElements: C
  ) where C: Collection, Self.Element == C.Element {
    precondition(subrange.lowerBound >= startIndex && subrange.lowerBound < endIndex)
    precondition(subrange.upperBound >= startIndex && subrange.upperBound <= endIndex)

    base.replaceSubrange(subrange, with: newElements)
  }

}
