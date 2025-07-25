//===--- Stack.swift - defines the Stack data structure -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

/// A very efficient implementation of a stack, which can also be iterated over.
///
/// A Stack is the best choice for things like worklists, etc., if no random
/// access is needed.
/// Compared to Array, it does not require any memory allocations, because it
/// uses a recycling bump pointer allocator for allocating the slabs.
/// All operations have (almost) zero cost.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct Stack<Element> : CollectionLikeSequence {

  private let bridgedContext: BridgedContext
  private var firstSlab = BridgedContext.Slab(nil)
  private var lastSlab = BridgedContext.Slab(nil)
  private var endIndex: Int = 0

  private static var slabCapacity: Int {
    BridgedContext.Slab.getCapacity() / MemoryLayout<Element>.stride
  }

  private func allocate(after lastSlab: BridgedContext.Slab? = nil) -> BridgedContext.Slab {
    let lastSlab = lastSlab ?? BridgedContext.Slab(nil)
    let newSlab = bridgedContext.allocSlab(lastSlab)
    UnsafeMutableRawPointer(newSlab.data!).bindMemory(to: Element.self, capacity: Stack.slabCapacity)
    return newSlab
  }

  private static func element(in slab: BridgedContext.Slab, at index: Int) -> Element {
    return pointer(in: slab, at: index).pointee
  }

  private static func pointer(in slab: BridgedContext.Slab, at index: Int) -> UnsafeMutablePointer<Element> {
    return UnsafeMutableRawPointer(slab.data!).assumingMemoryBound(to: Element.self) + index
  }

  struct Iterator : IteratorProtocol {
    var slab: BridgedContext.Slab
    var index: Int
    let lastSlab: BridgedContext.Slab
    let endIndex: Int
    
    mutating func next() -> Element? {
      let end = (slab.data == lastSlab.data ? endIndex : slabCapacity)
      
      guard index < end else { return nil }
    
      let elem = Stack.element(in: slab, at: index)
      index += 1
      
      if index >= end && slab.data != lastSlab.data {
        slab = slab.getNext()
        index = 0
      }
      return elem
    }
  }
  
  init(_ context: some Context) { self.bridgedContext = context._bridged }

  func makeIterator() -> Iterator {
    return Iterator(slab: firstSlab, index: 0, lastSlab: lastSlab, endIndex: endIndex)
  }

  var first: Element? {
    isEmpty ? nil : Stack.element(in: firstSlab, at: 0)
  }

  var last: Element? {
    isEmpty ? nil : Stack.element(in: lastSlab, at: endIndex &- 1)
  }

  mutating func push(_ element: Element) {
    if endIndex >= Stack.slabCapacity {
      lastSlab = allocate(after: lastSlab)
      endIndex = 0
    } else if firstSlab.data == nil {
      assert(endIndex == 0)
      firstSlab = allocate()
      lastSlab = firstSlab
    }
    Stack.pointer(in: lastSlab, at: endIndex).initialize(to: element)
    endIndex += 1
  }

  /// The same as `push` to provide an Array-like append API.
  mutating func append(_ element: Element) { push(element) }

  mutating func append<S: Sequence>(contentsOf other: S) where S.Element == Element {
    for elem in other {
      append(elem)
    }
  }

  var isEmpty: Bool { return endIndex == 0 }
  
  mutating func pop() -> Element? {
    if isEmpty {
      return nil
    }
    assert(endIndex > 0)
    endIndex -= 1
    let elem = Stack.pointer(in: lastSlab, at: endIndex).move()
    
    if endIndex == 0 {
      if lastSlab.data == firstSlab.data {
        _ = bridgedContext.freeSlab(lastSlab)
        firstSlab.data = nil
        lastSlab.data = nil
        endIndex = 0
      } else {
        lastSlab = bridgedContext.freeSlab(lastSlab)
        endIndex = Stack.slabCapacity
      }
    }

    return elem
  }
  
  mutating func removeAll() {
    while pop() != nil { }
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() { removeAll() }
}

extension Stack {
  /// Mark a stack location for future iteration.
  ///
  /// TODO: Marker should be ~Escapable.
  struct Marker {
    let slab: BridgedContext.Slab
    let index: Int
  }

  var top: Marker { Marker(slab: lastSlab, index: endIndex) }

  struct Segment : CollectionLikeSequence {
    let low: Marker
    let high: Marker

    init(in stack: Stack, low: Marker, high: Marker) {
      if low.slab.data == nil {
        assert(low.index == 0, "invalid empty stack marker")
        // `low == nil` and `high == nil` is a valid empty segment,
        // even though `assertValid(marker:)` would return false.
        if high.slab.data != nil {
          stack.assertValid(marker: high)
        }
        self.low = Marker(slab: stack.firstSlab, index: 0)
        self.high = high
        return
      }
      stack.assertValid(marker: low)
      stack.assertValid(marker: high)
      self.low = low
      self.high = high
    }

    func makeIterator() -> Stack.Iterator {
      return Iterator(slab: low.slab, index: low.index,
                      lastSlab: high.slab, endIndex: high.index)
    }
  }

  /// Assert that `marker` is valid based on the current `top`.
  ///
  /// This is an assert rather than a query because slabs can reuse
  /// memory leading to a stale marker that appears valid.
  func assertValid(marker: Marker) {
    var currentSlab = lastSlab
    var currentIndex = endIndex
    while currentSlab.data != marker.slab.data {
      assert(currentSlab.data != firstSlab.data, "Invalid stack marker")
      currentSlab = currentSlab.getPrevious()
      currentIndex = Stack.slabCapacity
    }
    assert(marker.index <= currentIndex, "Invalid stack marker")
  }

  /// Execute the `body` closure, passing it `self` for further
  /// mutation of the stack and passing `marker` to mark the stack
  /// position prior to executing `body`. `marker` must not escape the
  /// `body` closure.
  mutating func withMarker<R>(
    _ body: (inout Stack<Element>, Marker) throws -> R) rethrows -> R {
    return try body(&self, top)
  }

  /// Record a stack marker, execute a `body` closure, then execute a
  /// `handleNewElements` closure with the Segment that contains all
  /// elements that remain on the stack after being pushed on the
  /// stack while executing `body`. `body` must push more elements
  /// than it pops.
  mutating func withMarker<R>(
    pushElements body: (inout Stack) throws -> R,
    withNewElements handleNewElements: ((Segment) -> ())
  ) rethrows -> R {
    return try withMarker { (stack: inout Stack<Element>, marker: Marker) in
      let result = try body(&stack)
      handleNewElements(Segment(in: stack, low: marker, high: stack.top))
      return result
    }
  }
}
