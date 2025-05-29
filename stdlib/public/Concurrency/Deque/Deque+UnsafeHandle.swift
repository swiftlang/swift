//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

/// This file is copied from swift-collections and should not be modified here.
/// Rather all changes should be made to swift-collections and copied back.

import Swift

extension _Deque {
  @unsafe
  internal struct _UnsafeHandle {
    let _header: UnsafeMutablePointer<_DequeBufferHeader>
    let _elements: UnsafeMutablePointer<Element>
    #if DEBUG
    let _isMutable: Bool
    #endif

    init(
      header: UnsafeMutablePointer<_DequeBufferHeader>,
      elements: UnsafeMutablePointer<Element>,
      isMutable: Bool
    ) {
      unsafe self._header = unsafe header
      unsafe self._elements = unsafe elements
      #if DEBUG
      self._isMutable = isMutable
      #endif
    }
  }
}

extension _Deque._UnsafeHandle {
  func assertMutable() {
    #if DEBUG
    assert(_isMutable)
    #endif
  }
}

extension _Deque._UnsafeHandle {
  internal typealias Slot = _DequeSlot

  var header: _DequeBufferHeader {
    unsafe _header.pointee
  }

  var capacity: Int {
    unsafe _header.pointee.capacity
  }

  var count: Int {
    get { unsafe _header.pointee.count }
    nonmutating set { unsafe _header.pointee.count = newValue }
  }

  var startSlot: Slot {
    get { unsafe _header.pointee.startSlot }
    nonmutating set { unsafe _header.pointee.startSlot = newValue }
  }

  func ptr(at slot: Slot) -> UnsafeMutablePointer<Element> {
    unsafe assert(slot.position >= 0 && slot.position <= capacity)
    return unsafe _elements + slot.position
  }
}

extension _Deque._UnsafeHandle {
  var mutableBuffer: UnsafeMutableBufferPointer<Element> {
    unsafe assertMutable()
    return unsafe .init(start: _elements, count: _header.pointee.capacity)
  }

  internal func buffer(for range: Range<Slot>) -> UnsafeBufferPointer<Element> {
    unsafe assert(range.upperBound.position <= capacity)
    return unsafe .init(start: _elements + range.lowerBound.position, count: range._count)
  }

  internal func mutableBuffer(for range: Range<Slot>) -> UnsafeMutableBufferPointer<Element> {
    unsafe assertMutable()
    return unsafe .init(mutating: buffer(for: range))
  }
}

extension _Deque._UnsafeHandle {
  /// The slot immediately following the last valid one. (`endSlot` refers to
  /// the valid slot corresponding to `endIndex`, which is a different thing
  /// entirely.)
  internal var limSlot: Slot {
    unsafe Slot(at: capacity)
  }

  internal func slot(after slot: Slot) -> Slot {
    unsafe assert(slot.position < capacity)
    let position = slot.position + 1
    if unsafe position >= capacity {
      return Slot(at: 0)
    }
    return Slot(at: position)
  }

  internal func slot(before slot: Slot) -> Slot {
    unsafe assert(slot.position < capacity)
    if slot.position == 0 { return unsafe Slot(at: capacity - 1) }
    return Slot(at: slot.position - 1)
  }

  internal func slot(_ slot: Slot, offsetBy delta: Int) -> Slot {
    unsafe assert(slot.position <= capacity)
    let position = slot.position + delta
    if delta >= 0 {
      if unsafe position >= capacity { return unsafe Slot(at: position - capacity) }
    } else {
      if position < 0 { return unsafe Slot(at: position + capacity) }
    }
    return Slot(at: position)
  }

  internal var endSlot: Slot {
    unsafe slot(startSlot, offsetBy: count)
  }

  /// Return the storage slot corresponding to the specified offset, which may
  /// or may not address an existing element.
  internal func slot(forOffset offset: Int) -> Slot {
    assert(offset >= 0)
    unsafe assert(offset <= capacity) // Not `count`!

    // Note: The use of wrapping addition/subscription is justified here by the
    // fact that `offset` is guaranteed to fall in the range `0 ..< capacity`.
    // Eliminating the overflow checks leads to a measurable speedup for
    // random-access subscript operations. (Up to 2x on some microbenchmarks.)
    let position = unsafe startSlot.position &+ offset
    guard unsafe position < capacity else { return unsafe Slot(at: position &- capacity) }
    return Slot(at: position)
  }
}

extension _Deque._UnsafeHandle {
  internal func segments() -> _UnsafeWrappedBuffer<Element> {
    let wrap = unsafe capacity - startSlot.position
    if unsafe count <= wrap {
      return unsafe .init(start: ptr(at: startSlot), count: count)
    }
    return unsafe .init(first: ptr(at: startSlot), count: wrap,
                 second: ptr(at: .zero), count: count - wrap)
  }

  internal func segments(
    forOffsets offsets: Range<Int>
  ) -> _UnsafeWrappedBuffer<Element> {
    unsafe assert(offsets.lowerBound >= 0 && offsets.upperBound <= count)
    let lower = unsafe slot(forOffset: offsets.lowerBound)
    let upper = unsafe slot(forOffset: offsets.upperBound)
    if offsets.count == 0 || lower < upper {
      return unsafe .init(start: ptr(at: lower), count: offsets.count)
    }
    return unsafe .init(first: ptr(at: lower), count: capacity - lower.position,
                 second: ptr(at: .zero), count: upper.position)
  }

  internal func mutableSegments() -> _UnsafeMutableWrappedBuffer<Element> {
    unsafe assertMutable()
    return unsafe .init(mutating: segments())
  }

  internal func mutableSegments(
    forOffsets range: Range<Int>
  ) -> _UnsafeMutableWrappedBuffer<Element> {
    unsafe assertMutable()
    return unsafe .init(mutating: segments(forOffsets: range))
  }
}

extension _Deque._UnsafeHandle {
  internal func availableSegments() -> _UnsafeMutableWrappedBuffer<Element> {
    unsafe assertMutable()
    let endSlot = unsafe self.endSlot
    guard unsafe count < capacity else { return unsafe .init(start: ptr(at: endSlot), count: 0) }
    if unsafe endSlot < startSlot { return unsafe .init(mutableBuffer(for: endSlot ..< startSlot)) }
    return unsafe .init(mutableBuffer(for: endSlot ..< limSlot),
                 mutableBuffer(for: .zero ..< startSlot))
  }
}



extension _Deque._UnsafeHandle {
  @discardableResult
  func initialize(
    at start: Slot,
    from source: UnsafeBufferPointer<Element>
  ) -> Slot {
    unsafe assert(start.position + source.count <= capacity)
    guard source.count > 0 else { return start }
    unsafe ptr(at: start).initialize(from: source.baseAddress!, count: source.count)
    return Slot(at: start.position + source.count)
  }

  @discardableResult
  func moveInitialize(
    at start: Slot,
    from source: UnsafeMutableBufferPointer<Element>
  ) -> Slot {
    unsafe assert(start.position + source.count <= capacity)
    guard source.count > 0 else { return start }
    unsafe ptr(at: start).moveInitialize(from: source.baseAddress!, count: source.count)
    return Slot(at: start.position + source.count)
  }

  @discardableResult
  func move(
    from source: Slot,
    to target: Slot,
    count: Int
  ) -> (source: Slot, target: Slot) {
    assert(count >= 0)
    unsafe assert(source.position + count <= self.capacity)
    unsafe assert(target.position + count <= self.capacity)
    guard count > 0 else { return (source, target) }
    unsafe ptr(at: target).moveInitialize(from: ptr(at: source), count: count)
    return unsafe (slot(source, offsetBy: count), slot(target, offsetBy: count))
  }
}



extension _Deque._UnsafeHandle {
  /// Copy elements into a new storage instance without changing capacity or
  /// layout.
  internal func copyElements() -> _Deque._Storage {
    let object = unsafe _DequeBuffer<Element>.create(
      minimumCapacity: capacity,
      makingHeaderWith: { _ in unsafe header })
    let result = _Deque._Storage(_buffer: ManagedBufferPointer(unsafeBufferObject: object))
    guard unsafe self.count > 0 else { return result }
    unsafe result.update { target in
      let source = unsafe self.segments()
      unsafe target.initialize(at: startSlot, from: source.first)
      if let second = unsafe source.second {
        unsafe target.initialize(at: .zero, from: second)
      }
    }
    return result
  }

  /// Copy elements into a new storage instance with the specified minimum
  /// capacity.
  internal func copyElements(minimumCapacity: Int) -> _Deque._Storage {
    unsafe assert(minimumCapacity >= count)
    let object = _DequeBuffer<Element>.create(
      minimumCapacity: minimumCapacity,
      makingHeaderWith: {
        #if os(OpenBSD)
        let capacity = minimumCapacity
        #else
        let capacity = $0.capacity
        #endif
        return unsafe _DequeBufferHeader(
          capacity: capacity,
          count: count,
          startSlot: .zero)
      })
    let result = _Deque._Storage(_buffer: ManagedBufferPointer(unsafeBufferObject: object))
    guard unsafe count > 0 else { return result }
    unsafe result.update { target in
      unsafe assert(target.count == count && target.startSlot.position == 0)
      let source = unsafe self.segments()
      let next = unsafe target.initialize(at: .zero, from: source.first)
      if let second = unsafe source.second {
        unsafe target.initialize(at: next, from: second)
      }
    }
    return result
  }

  /// Move elements into a new storage instance with the specified minimum
  /// capacity. Existing indices in `self` won't necessarily be valid in the
  /// result. `self` is left empty.
  internal func moveElements(minimumCapacity: Int) -> _Deque._Storage {
    unsafe assertMutable()
    let count = unsafe self.count
    assert(minimumCapacity >= count)
    let object = _DequeBuffer<Element>.create(
      minimumCapacity: minimumCapacity,
      makingHeaderWith: {
        #if os(OpenBSD)
        let capacity = minimumCapacity
        #else
        let capacity = $0.capacity
        #endif
        return _DequeBufferHeader(
          capacity: capacity,
          count: count,
          startSlot: .zero)
      })
    let result = _Deque._Storage(_buffer: ManagedBufferPointer(unsafeBufferObject: object))
    guard count > 0 else { return result }
    unsafe result.update { target in
      let source = unsafe self.mutableSegments()
      let next = unsafe target.moveInitialize(at: .zero, from: source.first)
      if let second = unsafe source.second {
        unsafe target.moveInitialize(at: next, from: second)
      }
    }
    unsafe self.count = 0
    return result
  }
}

extension _Deque._UnsafeHandle {
  internal func withUnsafeSegment<R>(
    startingAt start: Int,
    maximumCount: Int?,
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> (end: Int, result: R) {
    unsafe assert(start <= count)
    guard unsafe start < count else {
      return unsafe try (count, body(UnsafeBufferPointer(start: nil, count: 0)))
    }
    let endSlot = unsafe self.endSlot

    let segmentStart = unsafe self.slot(forOffset: start)
    let segmentEnd = unsafe segmentStart < endSlot ? endSlot : limSlot
    let count = Swift.min(maximumCount ?? Int.max, segmentEnd.position - segmentStart.position)
    let result = try unsafe body(UnsafeBufferPointer(start: ptr(at: segmentStart), count: count))
    return (start + count, result)
  }
}

// MARK: Replacement

extension _Deque._UnsafeHandle {
  /// Replace the elements in `range` with `newElements`. The deque's count must
  /// not change as a result of calling this function.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func uncheckedReplaceInPlace<C: Collection>(
    inOffsets range: Range<Int>,
    with newElements: C
  ) where C.Element == Element {
    unsafe assertMutable()
    unsafe assert(range.upperBound <= count)
    assert(newElements.count == range.count)
    guard !range.isEmpty else { return }
    let target = unsafe mutableSegments(forOffsets: range)
    unsafe target.assign(from: newElements)
  }
}

// MARK: Appending

extension _Deque._UnsafeHandle {
  /// Append `element` to this buffer. The buffer must have enough free capacity
  /// to insert one new element.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func uncheckedAppend(_ element: Element) {
    unsafe assertMutable()
    unsafe assert(count < capacity)
    unsafe ptr(at: endSlot).initialize(to: element)
    unsafe count += 1
  }

  /// Append the contents of `source` to this buffer. The buffer must have
  /// enough free capacity to insert the new elements.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func uncheckedAppend(contentsOf source: UnsafeBufferPointer<Element>) {
    unsafe assertMutable()
    unsafe assert(count + source.count <= capacity)
    guard source.count > 0 else { return }
    let c = unsafe self.count
    unsafe count += source.count
    let gap = unsafe mutableSegments(forOffsets: c ..< count)
    unsafe gap.initialize(from: source)
  }
}

// MARK: Prepending

extension _Deque._UnsafeHandle {
  internal func uncheckedPrepend(_ element: Element) {
    unsafe assertMutable()
    unsafe assert(count < capacity)
    let slot = unsafe self.slot(before: startSlot)
    unsafe ptr(at: slot).initialize(to: element)
    unsafe startSlot = slot
    unsafe count += 1
  }

  /// Prepend the contents of `source` to this buffer. The buffer must have
  /// enough free capacity to insert the new elements.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func uncheckedPrepend(contentsOf source: UnsafeBufferPointer<Element>) {
    unsafe assertMutable()
    unsafe assert(count + source.count <= capacity)
    guard source.count > 0 else { return }
    let oldStart = unsafe startSlot
    let newStart = unsafe self.slot(startSlot, offsetBy: -source.count)
    unsafe startSlot = newStart
    unsafe count += source.count

    let gap = unsafe mutableWrappedBuffer(between: newStart, and: oldStart)
    unsafe gap.initialize(from: source)
  }
}

// MARK: Insertion

extension _Deque._UnsafeHandle {
  /// Insert all elements from `newElements` into this deque, starting at
  /// `offset`.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  ///
  /// - Parameter newElements: The elements to insert.
  /// - Parameter newCount: Must be equal to `newElements.count`. Used to
  ///    prevent calling `count` more than once.
  /// - Parameter offset: The desired offset from the start at which to place
  ///    the first element.
  internal func uncheckedInsert<C: Collection>(
    contentsOf newElements: __owned C,
    count newCount: Int,
    atOffset offset: Int
  ) where C.Element == Element {
    unsafe assertMutable()
    unsafe assert(offset <= count)
    assert(newElements.count == newCount)
    guard newCount > 0 else { return }
    let gap = unsafe openGap(ofSize: newCount, atOffset: offset)
    unsafe gap.initialize(from: newElements)
  }

  internal func mutableWrappedBuffer(
    between start: Slot,
    and end: Slot
  ) -> _UnsafeMutableWrappedBuffer<Element> {
    unsafe assert(start.position <= capacity)
    unsafe assert(end.position <= capacity)
    if start < end {
      return unsafe .init(start: ptr(at: start), count: end.position - start.position)
    }
    return unsafe .init(
      first: ptr(at: start), count: capacity - start.position,
      second: ptr(at: .zero), count: end.position)
  }

  /// Slide elements around so that there is a gap of uninitialized slots of
  /// size `gapSize` starting at `offset`, and return a (potentially wrapped)
  /// buffer holding the newly inserted slots.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  ///
  /// - Parameter gapSize: The number of uninitialized slots to create.
  /// - Parameter offset: The offset from the start at which the uninitialized
  ///    slots should start.
  internal func openGap(
    ofSize gapSize: Int,
    atOffset offset: Int
  ) -> _UnsafeMutableWrappedBuffer<Element> {
    unsafe assertMutable()
    unsafe assert(offset >= 0 && offset <= self.count)
    unsafe assert(self.count + gapSize <= capacity)
    assert(gapSize > 0)

    let headCount = offset
    let tailCount = unsafe count - offset
    if tailCount <= headCount {
      // Open the gap by sliding elements to the right.

      let originalEnd = unsafe self.slot(startSlot, offsetBy: count)
      let newEnd = unsafe self.slot(startSlot, offsetBy: count + gapSize)
      let gapStart = unsafe self.slot(forOffset: offset)
      let gapEnd = unsafe self.slot(gapStart, offsetBy: gapSize)

      let sourceIsContiguous = unsafe gapStart <= originalEnd.orIfZero(capacity)
      let targetIsContiguous = unsafe gapEnd <= newEnd.orIfZero(capacity)

      if sourceIsContiguous && targetIsContiguous {
        // No need to deal with wrapping; we just need to slide
        // elements after the gap.

        // Illustrated steps: (underscores mark eventual gap position)
        //
        //   0) ....ABCDE̲F̲G̲H.....      EFG̲H̲.̲........ABCD      .̲.......ABCDEFGH̲.̲
        //   1) ....ABCD.̲.̲.̲EFGH..      EF.̲.̲.̲GH......ABCD      .̲H......ABCDEFG.̲.̲
        unsafe move(from: gapStart, to: gapEnd, count: tailCount)
      } else if targetIsContiguous {
        // The gap itself will be wrapped.

        // Illustrated steps: (underscores mark eventual gap position)
        //
        //   0) E̲FGH.........ABC̲D̲
        //   1) .̲..EFGH......ABC̲D̲
        //   2) .̲CDEFGH......AB.̲.̲
        unsafe assert(startSlot > originalEnd.orIfZero(capacity))
        unsafe move(from: .zero, to: Slot.zero.advanced(by: gapSize), count: originalEnd.position)
        unsafe move(from: gapStart, to: gapEnd, count: capacity - gapStart.position)
      } else if sourceIsContiguous {
        // Opening the gap pushes subsequent elements across the wrap.

        // Illustrated steps: (underscores mark eventual gap position)
        //
        //   0) ........ABC̲D̲E̲FGH.
        //   1) GH......ABC̲D̲E̲F...
        //   2) GH......AB.̲.̲.̲CDEF
        unsafe move(from: limSlot.advanced(by: -gapSize), to: .zero, count: newEnd.position)
        unsafe move(from: gapStart, to: gapEnd, count: tailCount - newEnd.position)
      } else {
        // The rest of the items are wrapped, and will remain so.

        // Illustrated steps: (underscores mark eventual gap position)
        //
        //   0) GH.........AB̲C̲D̲EF
        //   1) ...GH......AB̲C̲D̲EF
        //   2) DEFGH......AB̲C̲.̲..
        //   3) DEFGH......A.̲.̲.̲BC
        unsafe move(from: .zero, to: Slot.zero.advanced(by: gapSize), count: originalEnd.position)
        unsafe move(from: limSlot.advanced(by: -gapSize), to: .zero, count: gapSize)
        unsafe move(from: gapStart, to: gapEnd, count: tailCount - gapSize - originalEnd.position)
      }
      unsafe count += gapSize
      return unsafe mutableWrappedBuffer(between: gapStart, and: gapEnd.orIfZero(capacity))
    }

    // Open the gap by sliding elements to the left.

    let originalStart = unsafe self.startSlot
    let newStart = unsafe self.slot(originalStart, offsetBy: -gapSize)
    let gapEnd = unsafe self.slot(forOffset: offset)
    let gapStart = unsafe self.slot(gapEnd, offsetBy: -gapSize)

    let sourceIsContiguous = unsafe originalStart <= gapEnd.orIfZero(capacity)
    let targetIsContiguous = unsafe newStart <= gapStart.orIfZero(capacity)

    if sourceIsContiguous && targetIsContiguous {
      // No need to deal with any wrapping.

      // Illustrated steps: (underscores mark eventual gap position)
      //
      //   0) ....A̲B̲C̲DEFGH...      GH.........̲A̲B̲CDEF      .̲A̲B̲CDEFGH.......̲.̲
      //   1) .ABC.̲.̲.̲DEFGH...      GH......AB.̲.̲.̲CDEF      .̲.̲.̲CDEFGH....AB.̲.̲
      unsafe move(from: originalStart, to: newStart, count: headCount)
    } else if targetIsContiguous {
      // The gap itself will be wrapped.

      // Illustrated steps: (underscores mark eventual gap position)
      //
      //   0) C̲D̲EFGH.........A̲B̲
      //   1) C̲D̲EFGH.....AB...̲.̲
      //   2) .̲.̲EFGH.....ABCD.̲.̲
      assert(originalStart >= newStart)
      unsafe move(from: originalStart, to: newStart, count: capacity - originalStart.position)
      unsafe move(from: .zero, to: limSlot.advanced(by: -gapSize), count: gapEnd.position)
    } else if sourceIsContiguous {
      // Opening the gap pushes preceding elements across the wrap.

      // Illustrated steps: (underscores mark eventual gap position)
      //
      //   0) .AB̲C̲D̲EFGH.........
      //   1) ...̲C̲D̲EFGH.......AB
      //   2) CD.̲.̲.̲EFGH.......AB
      unsafe move(from: originalStart, to: newStart, count: capacity - newStart.position)
      unsafe move(from: Slot.zero.advanced(by: gapSize), to: .zero, count: gapStart.position)
    } else {
      // The preceding of the items are wrapped, and will remain so.

      // Illustrated steps: (underscores mark eventual gap position)
      //   0) CD̲E̲F̲GHIJKL.........AB
      //   1) CD̲E̲F̲GHIJKL......AB...
      //   2) ..̲.̲F̲GHIJKL......ABCDE
      //   3) F.̲.̲.̲GHIJKL......ABCDE
      unsafe move(from: originalStart, to: newStart, count: capacity - originalStart.position)
      unsafe move(from: .zero, to: limSlot.advanced(by: -gapSize), count: gapSize)
      unsafe move(from: Slot.zero.advanced(by: gapSize), to: .zero, count: gapStart.position)
    }
    unsafe startSlot = newStart
    unsafe count += gapSize
    return unsafe mutableWrappedBuffer(between: gapStart, and: gapEnd.orIfZero(capacity))
  }
}

// MARK: Removal

extension _Deque._UnsafeHandle {
  internal func uncheckedRemoveFirst() -> Element {
    unsafe assertMutable()
    unsafe assert(count > 0)
    let result = unsafe ptr(at: startSlot).move()
    unsafe startSlot = unsafe slot(after: startSlot)
    unsafe count -= 1
    return result
  }

  internal func uncheckedRemoveLast() -> Element {
    unsafe assertMutable()
    unsafe assert(count > 0)
    let slot = unsafe self.slot(forOffset: count - 1)
    let result = unsafe ptr(at: slot).move()
    unsafe count -= 1
    return result
  }

  internal func uncheckedRemoveFirst(_ n: Int) {
    unsafe assertMutable()
    unsafe assert(count >= n)
    guard n > 0 else { return }
    let target = unsafe mutableSegments(forOffsets: 0 ..< n)
    unsafe target.deinitialize()
    unsafe startSlot = unsafe slot(startSlot, offsetBy: n)
    unsafe count -= n
  }

  internal func uncheckedRemoveLast(_ n: Int) {
    unsafe assertMutable()
    unsafe assert(count >= n)
    guard n > 0 else { return }
    let target = unsafe mutableSegments(forOffsets: count - n ..< count)
    unsafe target.deinitialize()
    unsafe count -= n
  }

  /// Remove all elements stored in this instance, deinitializing their storage.
  ///
  /// This method does not ensure that the storage buffer is uniquely
  /// referenced.
  internal func uncheckedRemoveAll() {
    unsafe assertMutable()
    guard unsafe count > 0 else { return }
    let target = unsafe mutableSegments()
    unsafe target.deinitialize()
    unsafe count = 0
    unsafe startSlot = .zero
  }

  /// Remove all elements in `bounds`, deinitializing their storage and sliding
  /// remaining elements to close the resulting gap.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func uncheckedRemove(offsets bounds: Range<Int>) {
    unsafe assertMutable()
    unsafe assert(bounds.lowerBound >= 0 && bounds.upperBound <= self.count)

    // Deinitialize elements in `bounds`.
    unsafe mutableSegments(forOffsets: bounds).deinitialize()
    unsafe closeGap(offsets: bounds)
  }

  /// Close the gap of already uninitialized elements in `bounds`, sliding
  /// elements outside of the gap to eliminate it.
  ///
  /// This function does not validate its input arguments in release builds. Nor
  /// does it ensure that the storage buffer is uniquely referenced.
  internal func closeGap(offsets bounds: Range<Int>) {
    unsafe assertMutable()
    unsafe assert(bounds.lowerBound >= 0 && bounds.upperBound <= self.count)
    let gapSize = bounds.count
    guard gapSize > 0 else { return }

    let gapStart = unsafe self.slot(forOffset: bounds.lowerBound)
    let gapEnd = unsafe self.slot(forOffset: bounds.upperBound)

    let headCount = bounds.lowerBound
    let tailCount = unsafe count - bounds.upperBound

    if headCount >= tailCount {
      // Close the gap by sliding elements to the left.
      let originalEnd = unsafe endSlot
      let newEnd = unsafe self.slot(forOffset: count - gapSize)

      let sourceIsContiguous = unsafe gapEnd < originalEnd.orIfZero(capacity)
      let targetIsContiguous = unsafe gapStart <= newEnd.orIfZero(capacity)
      if tailCount == 0 {
        // No need to move any elements.
      } else if sourceIsContiguous && targetIsContiguous {
        // No need to deal with wrapping.

        //   0) ....ABCD.̲.̲.̲EFGH..   EF.̲.̲.̲GH........ABCD   .̲.̲.̲E..........ABCD.̲.̲   .̲.̲.̲EF........ABCD .̲.̲.̲DE.......ABC
        //   1) ....ABCDE̲F̲G̲H.....   EFG̲H̲.̲..........ABCD   .̲.̲.̲...........ABCDE̲.̲   E̲F̲.̲..........ABCD D̲E̲.̲.........ABC
        unsafe move(from: gapEnd, to: gapStart, count: tailCount)
      } else if sourceIsContiguous {
        // The gap lies across the wrap from the subsequent elements.

        //   0) .̲.̲.̲EFGH.......ABCD.̲.̲      EFGH.......ABCD.̲.̲.̲
        //   1) .̲.̲.̲..GH.......ABCDE̲F̲      ..GH.......ABCDE̲F̲G̲
        //   2) G̲H̲.̲...........ABCDE̲F̲      GH.........ABCDE̲F̲G̲
        let c = unsafe capacity - gapStart.position
        assert(tailCount > c)
        let next = unsafe move(from: gapEnd, to: gapStart, count: c)
        unsafe move(from: next.source, to: .zero, count: tailCount - c)
      } else if targetIsContiguous {
        // We need to move elements across a wrap, but the wrap will
        // disappear when we're done.

        //   0) HI....ABCDE.̲.̲.̲FG
        //   1) HI....ABCDEF̲G̲.̲..
        //   2) ......ABCDEF̲G̲H̲I.
        let next = unsafe move(from: gapEnd, to: gapStart, count: capacity - gapEnd.position)
        unsafe move(from: .zero, to: next.target, count: originalEnd.position)
      } else {
        // We need to move elements across a wrap that won't go away.

        //   0) HIJKL....ABCDE.̲.̲.̲FG
        //   1) HIJKL....ABCDEF̲G̲.̲..
        //   2) ...KL....ABCDEF̲G̲H̲IJ
        //   3) KL.......ABCDEF̲G̲H̲IJ
        var next = unsafe move(from: gapEnd, to: gapStart, count: capacity - gapEnd.position)
        next = unsafe move(from: .zero, to: next.target, count: gapSize)
        unsafe move(from: next.source, to: .zero, count: newEnd.position)
      }
      unsafe count -= gapSize
    } else {
      // Close the gap by sliding elements to the right.
      let originalStart = unsafe startSlot
      let newStart = unsafe slot(startSlot, offsetBy: gapSize)

      let sourceIsContiguous = unsafe originalStart < gapStart.orIfZero(capacity)
      let targetIsContiguous = unsafe newStart <= gapEnd.orIfZero(capacity)

      if headCount == 0 {
        // No need to move any elements.
      } else if sourceIsContiguous && targetIsContiguous {
        // No need to deal with wrapping.

        //   0) ....ABCD.̲.̲.̲EFGH.....   EFGH........AB.̲.̲.̲CD   .̲.̲.̲CDEFGH.......AB.̲.̲   DEFGH.......ABC.̲.̲
        //   1) .......AB̲C̲D̲EFGH.....   EFGH...........̲A̲B̲CD   .̲A̲B̲CDEFGH..........̲.̲   DEFGH.........AB̲C̲     ABCDEFGH........̲.̲.̲
        unsafe move(from: originalStart, to: newStart, count: headCount)
      } else if sourceIsContiguous {
        // The gap lies across the wrap from the preceding elements.

        //   0) .̲.̲DEFGH.......ABC.̲.̲     .̲.̲.̲EFGH.......ABCD
        //   1) B̲C̲DEFGH.......A...̲.̲     B̲C̲D̲DEFGH......A...
        //   2) B̲C̲DEFGH...........̲A̲     B̲C̲D̲DEFGH.........A
        unsafe move(from: limSlot.advanced(by: -gapSize), to: .zero, count: gapEnd.position)
        unsafe move(from: startSlot, to: newStart, count: headCount - gapEnd.position)
      } else if targetIsContiguous {
        // We need to move elements across a wrap, but the wrap will
        // disappear when we're done.

        //   0) CD.̲.̲.̲EFGHI.....AB
        //   1) ...̲C̲D̲EFGHI.....AB
        //   1) .AB̲C̲D̲EFGHI.......
        unsafe move(from: .zero, to: gapEnd.advanced(by: -gapStart.position), count: gapStart.position)
        unsafe move(from: startSlot, to: newStart, count: headCount - gapStart.position)
      } else {
        // We need to move elements across a wrap that won't go away.
        //   0) FG.̲.̲.̲HIJKLMNO....ABCDE
        //   1) ...̲F̲G̲HIJKLMNO....ABCDE
        //   2) CDE̲F̲G̲HIJKLMNO....AB...
        //   3) CDE̲F̲G̲HIJKLMNO.......AB
        unsafe move(from: .zero, to: Slot.zero.advanced(by: gapSize), count: gapStart.position)
        unsafe move(from: limSlot.advanced(by: -gapSize), to: .zero, count: gapSize)
        unsafe move(from: startSlot, to: newStart, count: headCount - gapEnd.position)
      }
      unsafe startSlot = newStart
      unsafe count -= gapSize
    }
  }
}
