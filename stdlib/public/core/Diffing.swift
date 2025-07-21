//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2015 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// MARK: Diff application to RangeReplaceableCollection

@available(SwiftStdlib 5.1, *)
extension CollectionDifference {
  fileprivate func _fastEnumeratedApply(
    _ consume: (Change) throws -> Void
  ) rethrows {
    let totalRemoves = removals.count
    let totalInserts = insertions.count
    var enumeratedRemoves = 0
    var enumeratedInserts = 0

    while enumeratedRemoves < totalRemoves || enumeratedInserts < totalInserts {
      let change: Change
      if enumeratedRemoves < removals.count && enumeratedInserts < insertions.count {
        let removeOffset = removals[enumeratedRemoves]._offset
        let insertOffset = insertions[enumeratedInserts]._offset
        if removeOffset - enumeratedRemoves <= insertOffset - enumeratedInserts {
          change = removals[enumeratedRemoves]
        } else {
          change = insertions[enumeratedInserts]
        }
      } else if enumeratedRemoves < totalRemoves {
        change = removals[enumeratedRemoves]
      } else if enumeratedInserts < totalInserts {
        change = insertions[enumeratedInserts]
      } else {
        // Not reached, loop should have exited.
        preconditionFailure()
      }

      try consume(change)

      switch change {
      case .remove(_, _, _):
        enumeratedRemoves += 1
      case .insert(_, _, _):
        enumeratedInserts += 1
      }
    }
  }
}

// Error type allows the use of throw to unroll state on application failure
private enum _ApplicationError : Error { case failed }

extension RangeReplaceableCollection {
  /// Applies the given difference to this collection.
  ///
  /// - Parameter difference: The difference to be applied.
  ///
  /// - Returns: An instance representing the state of the receiver with the
  ///   difference applied, or `nil` if the difference is incompatible with
  ///   the receiver's state.
  ///
  /// - Complexity: O(*n* + *c*), where *n* is `self.count` and *c* is the
  ///   number of changes contained by the parameter.
  @available(SwiftStdlib 5.1, *)
  public func applying(_ difference: CollectionDifference<Element>) -> Self? {

    func append(
      into target: inout Self,
      contentsOf source: Self,
      from index: inout Self.Index, count: Int
    ) throws {
      let start = index
      if !source.formIndex(&index, offsetBy: count, limitedBy: source.endIndex) {
        throw _ApplicationError.failed
      }
      target.append(contentsOf: source[start..<index])
    }

    var result = Self()
    do {
      var enumeratedRemoves = 0
      var enumeratedInserts = 0
      var enumeratedOriginals = 0
      var currentIndex = self.startIndex
      try difference._fastEnumeratedApply { change in
        switch change {
        case .remove(offset: let offset, element: _, associatedWith: _):
          let origCount = offset - enumeratedOriginals
          try append(into: &result, contentsOf: self, from: &currentIndex, count: origCount)
          if currentIndex == self.endIndex {
            // Removing nonexistent element off the end of the collection
            throw _ApplicationError.failed
          }
          enumeratedOriginals += origCount + 1 // Removal consumes an original element
          currentIndex = self.index(after: currentIndex)
          enumeratedRemoves += 1
        case .insert(offset: let offset, element: let element, associatedWith: _):
          let origCount = (offset + enumeratedRemoves - enumeratedInserts) - enumeratedOriginals
          try append(into: &result, contentsOf: self, from: &currentIndex, count: origCount)
          result.append(element)
          enumeratedOriginals += origCount
          enumeratedInserts += 1
        }
        _internalInvariant(enumeratedOriginals <= self.count)
      }
      if currentIndex < self.endIndex {
        result.append(contentsOf: self[currentIndex...])
      }
      _internalInvariant(result.count == self.count + enumeratedInserts - enumeratedRemoves)
    } catch {
      return nil
    }

    return result
  }
}

// MARK: Definition of API

extension BidirectionalCollection {
  /// Returns the difference needed to produce this collection's ordered
  /// elements from the given collection, using the given predicate as an
  /// equivalence test.
  ///
  /// This function does not infer element moves. If you need to infer moves,
  /// call the `inferringMoves()` method on the resulting difference.
  ///
  /// - Parameters:
  ///   - other: The base state.
  ///   - areEquivalent: A closure that returns a Boolean value indicating
  ///     whether two elements are equivalent.
  ///
  /// - Returns: The difference needed to produce the receiver's state from
  ///   the parameter's state.
  ///
  /// - Complexity: Worst case performance is O(*n* * *m*), where *n* is the
  ///   count of this collection and *m* is `other.count`. You can expect
  ///   faster execution when the collections share many common elements.
  @available(SwiftStdlib 5.1, *)
  public func difference<C: BidirectionalCollection>(
    from other: C,
    by areEquivalent: (C.Element, Element) -> Bool
  ) -> CollectionDifference<Element>
  where C.Element == Self.Element {
    _linearSpaceMyers(from: other, to: self, using: areEquivalent)
  }
}

extension BidirectionalCollection where Element: Equatable {
  /// Returns the difference needed to produce this collection's ordered
  /// elements from the given collection.
  ///
  /// This function does not infer element moves. If you need to infer moves,
  /// call the `inferringMoves()` method on the resulting difference.
  ///
  /// - Parameters:
  ///   - other: The base state.
  ///
  /// - Returns: The difference needed to produce this collection's ordered
  ///   elements from the given collection.
  ///
  /// - Complexity: Worst case performance is O(*n* * *m*), where *n* is the
  ///   count of this collection and *m* is `other.count`. You can expect
  ///   faster execution when the collections share many common elements, or
  ///   if `Element` conforms to `Hashable`.
  @available(SwiftStdlib 5.1, *)
  public func difference<C: BidirectionalCollection>(
    from other: C
  ) -> CollectionDifference<Element> where C.Element == Self.Element {
    difference(from: other, by: ==)
  }
}

// MARK: Internal implementation

/// Storage for the two "k-vectors" used by the Myers diffing
/// algorithm, using a single allocation.
///
/// The two k-vectors are stored one after the other in this type's
/// buffer. For a given size `s`, each k-vector provides space for
/// offsets in the range `-s...s`, using this layout for the buffer
/// and offsets when `s == 2`:
///
/// ```
/// ┌─────────────────────────────────────────────────────────────┐
/// │             0    1    2    3    4    5    6    7    8    9  │
/// │          ┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐│
/// │ buffer:  │ .. │ .. │ .. │ .. │ .. │ .. │ .. │ .. │ .. │ .. ││
/// │          └────┴────┴────┴────┴────┴────┴────┴────┴────┴────┘│
/// |                       ^                        ^            |
/// |                  forwardOffset           backwardOffset     |
/// │          ┌────┬────┬────┬────┬────┐                         │
/// │ forward: │ -2 │ -1 │  0 │  1 │  2 │                         │
/// │          └────┴────┴────┴────┴────┘                         │
/// │                                   ┌────┬────┬────┬────┬────┐│
/// │ backward:                         │ -2 │ -1 │  0 │  1 │  2 ││
/// │                                   └────┴────┴────┴────┴────┘│
/// └─────────────────────────────────────────────────────────────┘
/// ```
@safe fileprivate struct DoubleKVector: ~Copyable {
  let buffer: UnsafeMutableBufferPointer<Int>
  let forwardOffset: Int
  let backwardOffset: Int
#if INTERNAL_CHECKS_ENABLED
  private let range: Int
#endif

  /// Creates a new double k-vector.
  ///
  /// The size of the resulting array is `((2 * size) + 1) * 2`,
  /// enough space to store two k-vectors ranging from `-size`
  /// through `size`.
  init(size: Int) {
    let range = size * 2 + 1
    unsafe self.buffer = .allocate(capacity: range * 2)
    self.forwardOffset = size
    self.backwardOffset = range + size
    unsafe buffer.initialize(repeating: 0)
#if INTERNAL_CHECKS_ENABLED
    self.range = range
#endif
  }

  deinit {
    unsafe buffer.deallocate()
  }
  
  @inline(__always) fileprivate func forwardTransform(_ index: Int) -> Int {
    forwardOffset &+ index
  }

  @inline(__always) fileprivate func backwardTransform(_ index: Int) -> Int {
    backwardOffset &+ index
  }
  
  subscript(forward index: Int) -> Int {
    get {
#if INTERNAL_CHECKS_ENABLED
      precondition(((-range)...range).contains(index))
#endif
      return unsafe buffer[forwardTransform(index)]
    }
    set(newValue) {
#if INTERNAL_CHECKS_ENABLED
      precondition(((-range)...range).contains(index))
#endif
      unsafe buffer[forwardTransform(index)] = newValue
    }
  }
  
  subscript(backward index: Int) -> Int {
    get {
#if INTERNAL_CHECKS_ENABLED
      precondition(((-range)...range).contains(index))
#endif
      return unsafe buffer[backwardTransform(index)]
    }
    set(newValue) {
#if INTERNAL_CHECKS_ENABLED
      precondition(((-range)...range).contains(index))
#endif
      unsafe buffer[backwardTransform(index)] = newValue
    }
  }
}

/// A two-dimensional region of a Myers diff algorithm edit graph.
fileprivate struct EditGraphRect: Equatable {
  var left: Int
  var top: Int
  var right: Int
  var bottom: Int
  
  var width: Int {
    right &- left
  }

  var height: Int {
    bottom &- top
  }

  var size: Int {
    width &+ height
  }

  var delta: Int {
    width &- height
  }
  
  var isEven: Bool {
    delta.isMultiple(of: 2)
  }
  
  var isOdd: Bool {
    !delta.isMultiple(of: 2)
  }
  
  var max: Int {
    (size &+ 1) / 2
  }
  
  /// Shrinks this box so that its bottom right corner is the top left corner of
  /// the given box.
  func cropped(toTopLeftOf limit: EditGraphRect) -> EditGraphRect {
    .init(left: left, top: top, right: limit.left, bottom: limit.top)
  }
  
  /// Shrinks this box so that its top left corner is the bottom right corner of
  /// the given box.
  func cropped(toBottomRightOf limit: EditGraphRect) -> EditGraphRect {
    .init(left: limit.right, top: limit.bottom, right: right, bottom: bottom)
  }

  /// Shrinks this box from both top and bottom by following diagonal paths
  /// (equal corresponding elements) in the two collections that are the target
  /// of diffing.
  func shrunk<T>(
    old: UnsafeBufferPointer<T>,
    new: UnsafeBufferPointer<T>,
    cmp: (T, T) -> Bool
  ) -> EditGraphRect {
    var r = self
    while r.left < r.right, r.top < r.bottom,
          unsafe cmp(old[r.left], new[r.top])
    {
      r.left &+= 1
      r.top &+= 1
    }
    while r.right > r.left, r.bottom > r.top,
          unsafe cmp(old[r.right - 1], new[r.bottom - 1])
    {
      r.right &-= 1
      r.bottom &-= 1
    }
    return r
  }
}

fileprivate struct LinearMyers: ~Copyable {
  var kVector: DoubleKVector
  
  /// Implements a refinement of the Myers diffing algorithm that uses
  /// linear space for storing the "k vectors" during an iterative divide-
  /// and-conquer search.
  mutating func findDifferences<T>(
    in initial: EditGraphRect,
    old: UnsafeBufferPointer<T>,
    new: UnsafeBufferPointer<T>,
    cmp: (T, T) -> Bool
  ) -> [CollectionDifference<T>.Change] {
    var result: [CollectionDifference<T>.Change] = []
    var stack: [EditGraphRect] = []
    var current = initial
    
    while true {
      let (edit, snakeBox) = unsafe middleSnake(
        in: current, old: old, new: new, cmp: cmp)
      
      // Append edit if exists
      if let edit {
        result.append(edit)
      }
      
      guard let snakeBox else {
        // Didn't find a new middle snake, so we'll look at the next area on the
        // stack next (the right side of the last snake)
        guard let next = stack.popLast() else {
          // Stack is empty -- all done with diff!
          return result
        }
        current = next
        continue
      }
      
      let headBox = unsafe current.cropped(toTopLeftOf: snakeBox)
        .shrunk(old: old, new: new, cmp: cmp)
      let tailBox = unsafe current.cropped(toBottomRightOf: snakeBox)
        .shrunk(old: old, new: new, cmp: cmp)
      
      // Process the left side of the snake next
      current = headBox
      // Save the right side of the snake for later
      stack.append(tailBox)
    }
  }
  
  fileprivate mutating func middleSnake<T>(
    in box: EditGraphRect,
    old: UnsafeBufferPointer<T>, new: UnsafeBufferPointer<T>,
    cmp: (T, T) -> Bool
  ) -> (CollectionDifference<T>.Change?, EditGraphRect?) {
    guard box.size > 1 else {
      // One rightward or downward move represents a removal or insertion.
      if box.size == 0 {
        return (nil, nil)
      } else if box.width == 1 {
        return unsafe (.remove(offset: box.left, element: old[box.left], associatedWith: nil), nil)
      } else {
        return unsafe (.insert(offset: box.top, element: new[box.top], associatedWith: nil), nil)
      }
    }
    
    // Reset for depth == 0
    kVector[forward: 1] = box.left
    kVector[backward: 1] = box.bottom
    
    for depth in 0...box.max {
      if box.isOdd {
        if let result = unsafe forwardSearch(
          in: box, depth: depth, old: old, new: new, cmp: cmp)
        {
          return result
        }
        _ = unsafe backwardSearch(in: box, depth: depth, old: old, new: new, cmp: cmp)
      } else {
        _ = unsafe forwardSearch(in: box, depth: depth, old: old, new: new, cmp: cmp)
        if let result = unsafe backwardSearch(
          in: box, depth: depth, old: old, new: new, cmp: cmp)
        {
          return result
        }
      }
    }
    fatalError("Unreachable")
  }
  
  fileprivate mutating func forwardSearch<T>(
    in box: EditGraphRect,
    depth: Int,
    old: UnsafeBufferPointer<T>,
    new: UnsafeBufferPointer<T>,
    cmp: (T, T) -> Bool
  ) -> (CollectionDifference<T>.Change, EditGraphRect)? {
    for k in stride(from: depth, through: -depth, by: -2) {
      // The furthest right and down we search
      var newRight, newBottom: Int
      // The top/left of the candidate middle snake
      let newLeft, newTop: Int
      
      // Information about the selected edit, if used
      let isInsertion: Bool
      let editIndex: Int
            
      // Choose whether this is an insertion or a deletion, then set up
      // bounds of candidate middle snake.
      if k == -depth ||
          (k != depth && kVector[forward: k - 1] < kVector[forward: k + 1])
      {
        newRight = kVector[forward: k + 1]
        newLeft = newRight
        newBottom = box.top + (newRight - box.left) - k
        
        isInsertion = true
        editIndex = newBottom - 1
      } else {
        newLeft = kVector[forward: k - 1]
        newRight = newLeft + 1
        newBottom = box.top + (newRight - box.left) - k
        
        isInsertion = false
        editIndex = newLeft
      }
      newTop = if depth == 0 || newRight != newLeft {
        newBottom
      } else {
        newBottom - 1
      }
      
      // Follow any diagonal after the movement
      while newRight < box.right && newBottom < box.bottom,
            unsafe cmp(old[newRight], new[newBottom])
      {
        newRight &+= 1
        newBottom &+= 1
      }
      kVector[forward: k] = newRight
      
      // Only check for overlap in _odd-sized_ boxes when moving _forward_
      guard box.isOdd else { continue }
      
      // If `c` is in bounds and there's an overlap, return the identified edit
      // and the eliminated box.
      let c = k - box.delta
      if (c >= -(depth - 1) && c <= depth - 1) && newBottom >= kVector[backward: c] {
        let edit: CollectionDifference<T>.Change = if isInsertion {
          unsafe .insert(offset: editIndex, element: new[editIndex], associatedWith: nil)
        } else {
          unsafe .remove(offset: editIndex, element: old[editIndex], associatedWith: nil)
        }
        return (
          edit,
          EditGraphRect(left: newLeft, top: newTop, right: newRight, bottom: newBottom))
      }
    }
    return nil
  }
    
  fileprivate mutating func backwardSearch<T>(
    in box: EditGraphRect,
    depth: Int,
    old: UnsafeBufferPointer<T>,
    new: UnsafeBufferPointer<T>,
    cmp: (T, T) -> Bool
  ) -> (CollectionDifference<T>.Change, EditGraphRect)? {
    for c in stride(from: depth, through: -depth, by: -2) {
      let k = c + box.delta
      // The furthest up and left we search
      var newLeft, newTop: Int
      // The bottom/right of the candidate middle snake
      let newRight, newBottom: Int
      
      // Information about the selected edit, if used
      let isInsertion: Bool
      let editIndex: Int
      
      // Choose whether this is an insertion or a deletion, then set up
      // bounds of candidate middle snake.
      if c == -depth ||
          (c != depth && kVector[backward: c - 1] > kVector[backward: c + 1])
      {
        newTop = kVector[backward: c + 1]
        newBottom = newTop
        newLeft = box.left + (newTop - box.top) + k
        
        isInsertion = false
        editIndex = newLeft
      } else {
        newBottom = kVector[backward: c - 1]
        newTop = newBottom - 1
        newLeft = box.left + (newTop - box.top) + k

        isInsertion = true
        editIndex = newTop
      }
      newRight = if depth == 0 || newTop != newBottom {
        newLeft
      } else {
        newLeft + 1
      }
      
      // Follow any diagonal after the movement
      while newLeft > box.left && newTop > box.top,
            unsafe cmp(old[newLeft - 1], new[newTop - 1])
      {
        newLeft &-= 1
        newTop &-= 1
      }
      kVector[backward: c] = newTop
      
      // Only check for overlap in _even-sized_ boxes when moving _backward_
      guard box.isEven else { continue }
      
      // If `k` is in bounds and there's an overlap, return the identified edit
      // and the eliminated box.
      if (k >= -depth && k <= depth) && newLeft <= kVector[forward: k] {
        let edit: CollectionDifference<T>.Change = if isInsertion {
          unsafe .insert(offset: editIndex, element: new[editIndex], associatedWith: nil)
        } else {
          unsafe .remove(offset: editIndex, element: old[editIndex], associatedWith: nil)
        }
        return (
          edit,
          EditGraphRect(left: newLeft, top: newTop, right: newRight, bottom: newBottom))
      }
    }
    return nil
  }
}

extension LinearMyers {
  fileprivate init?(
    initialSize size: Int
  ) {
    guard size >= 0 else { return nil }
    self.kVector = DoubleKVector(size: size)
  }
}

func _linearSpaceMyers<C,D>(
  from old: C, to new: D,
  using cmp: (C.Element, D.Element) -> Bool
) -> CollectionDifference<C.Element>
  where
    C: BidirectionalCollection,
    D: BidirectionalCollection,
    C.Element == D.Element
{
  /* Splatting the collections into contiguous storage has two advantages:
   *
   *   1) Subscript access is much faster
   *   2) Subscript index becomes Int, matching the iterator types in the algorithm
   *
   * Combined, these effects dramatically improve performance when
   * collections differ significantly, without unduly degrading runtime when
   * the parameters are very similar.
   */
  func _withContiguousStorage<Col: Collection, R>(
    for values: Col,
    _ body: (UnsafeBufferPointer<Col.Element>) -> R
  ) -> R {
    if let result = values.withContiguousStorageIfAvailable(body) { return result }
    let array = ContiguousArray(values)
    return unsafe array.withUnsafeBufferPointer(body)
  }

  return unsafe _withContiguousStorage(for: old) { a in
    return unsafe _withContiguousStorage(for: new) { b in
      let box = unsafe EditGraphRect(left: 0, top: 0, right: old.count, bottom: new.count)
        .shrunk(old: a, new: b, cmp: cmp)
      
      guard var myers = LinearMyers(initialSize: box.size) else {
        return CollectionDifference([])!
      }
      let diffs = unsafe myers.findDifferences(in: box, old: a, new: b, cmp: cmp)
      return CollectionDifference(diffs)!
    }
  }
}
