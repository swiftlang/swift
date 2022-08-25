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

  private let bridgedContext: BridgedPassContext
  private var firstSlab = BridgedSlab(data: nil)
  private var lastSlab = BridgedSlab(data: nil)
  private var endIndex: Int = 0

  private static var slabCapacity: Int {
    BridgedSlabCapacity / MemoryLayout<Element>.stride
  }

  private static func bind(_ slab: BridgedSlab) -> UnsafeMutablePointer<Element> {
    return slab.data!.bindMemory(to: Element.self, capacity: Stack.slabCapacity)
  }

  struct Iterator : IteratorProtocol {
    var slab: BridgedSlab
    var index: Int
    let lastSlab: BridgedSlab
    let endIndex: Int
    
    mutating func next() -> Element? {
      let end = (slab.data == lastSlab.data ? endIndex : slabCapacity)
      if index < end {
        let elem = Stack.bind(slab)[index]
        index += 1

        if index >= end && slab.data != lastSlab.data {
          slab = PassContext_getNextSlab(slab)
          index = 0
        }
        return elem
      }
      return nil
    }
  }
  
  init(_ context: PassContext) { self.bridgedContext = context._bridged }
  init(_ context: ModulePassContext) { self.bridgedContext = context._bridged }

  func makeIterator() -> Iterator {
    return Iterator(slab: firstSlab, index: 0, lastSlab: lastSlab, endIndex: endIndex)
  }

  var first: Element? {
    isEmpty ? nil : Stack.bind(firstSlab)[0]
  }

  var last: Element? {
    isEmpty ? nil : Stack.bind(lastSlab)[endIndex &- 1]
  }

  mutating func push(_ element: Element) {
    if endIndex >= Stack.slabCapacity {
      lastSlab = PassContext_allocSlab(bridgedContext, lastSlab)
      endIndex = 0
    } else if firstSlab.data == nil {
      assert(endIndex == 0)
      firstSlab = PassContext_allocSlab(bridgedContext, lastSlab)
      lastSlab = firstSlab
    }
    (Stack.bind(lastSlab) + endIndex).initialize(to: element)
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
    let elem = (Stack.bind(lastSlab) + endIndex).move()
    
    if endIndex == 0 {
      if lastSlab.data == firstSlab.data {
        _ = PassContext_freeSlab(bridgedContext, lastSlab)
        firstSlab.data = nil
        lastSlab.data = nil
        endIndex = 0
      } else {
        lastSlab = PassContext_freeSlab(bridgedContext, lastSlab)
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
