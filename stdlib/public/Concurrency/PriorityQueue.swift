//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A generic priority queue, with a user-defined comparison function.
struct PriorityQueue<T> {
  /// The backing store.
  var storage: [T] = []

  /// The comparison function; this should compare the priorities of
  /// two items in the queue, returning `true` if they satisfy the
  /// desired condition.
  ///
  /// A `>` test will produce a max-queue, while a `<` test will
  /// result in a min-queue.
  var compare: (borrowing T, borrowing T) -> Bool

  /// Initialise the queue.
  ///
  /// The queue is set up such that if the comparison function in use
  /// is `>`, it will be a max-queue, while if the comparison function
  /// is `<`, it will be a min-queue.
  ///
  /// Parameters:
  ///
  /// - compare: A closure that takes two arguments of type T, and
  ///            returns true if some condition holds.
  ///
  init(compare: @escaping (borrowing T, borrowing T) -> Bool) {
    self.compare = compare
  }

  /// Push an item onto the queue.
  ///
  /// Parameters:
  ///
  /// - _ value: The item to push onto the queue.
  ///
  mutating func push(_ value: T) {
    storage.append(value)
    upHeap(ndx: storage.count - 1)
  }

  /// The highest priority item from the queue, or `nil` if none.
  var top: T? {
    if storage.isEmpty {
      return nil
    }
    return storage[0]
  }

  /// Conditionally pop the highest priority item from the queue.
  ///
  /// If the comparison function is `>`, this will return the largest
  /// item in the queue.  If the comparison function is `<`, it will
  /// return the smallest.
  ///
  /// Parameters:
  ///
  /// - when: A closure that allows code to test the top item before
  ///         popping.
  ///
  /// Returns: The next item in the queue, following the comparison
  ///          rule.
  ///
  mutating func pop(when condition: (borrowing T) -> Bool) -> T? {
    if storage.isEmpty {
      return nil
    }
    if !condition(storage[0]) {
      return nil
    }
    storage.swapAt(0, storage.count - 1)
    let result = storage.removeLast()
    if !storage.isEmpty {
      downHeap(ndx: 0)
    }
    return result
  }

  /// Pop the highest priority item from the queue.
  ///
  /// If the comparison function is `>`, this will return the largest
  /// item in the queue.  If the comparison function is `<`, it will
  /// return the smallest.
  ///
  /// Returns: The next item in the queue, following the comparison
  ///          rule.
  ///
  mutating func pop() -> T? {
    if storage.isEmpty {
      return nil
    }
    storage.swapAt(0, storage.count - 1)
    let result = storage.removeLast()
    if !storage.isEmpty {
      downHeap(ndx: 0)
    }
    return result
  }

  /// Fix the heap condition by iterating upwards.
  ///
  /// Iterate upwards from the specified entry, exchanging it with
  /// its parent if the parent and child do not satisfy the comparison
  /// function.
  ///
  /// This is used when pushing items onto the queue.
  ///
  /// Parameters:
  ///
  /// - ndx: The index at which to start.
  ///
  private mutating func upHeap(ndx: Int) {
    var theNdx = ndx
    while theNdx > 0 {
      let parentNdx = (theNdx - 1) / 2

      if !compare(storage[theNdx], storage[parentNdx]) {
        break
      }

      storage.swapAt(theNdx, parentNdx)
      theNdx = parentNdx
    }
  }

  /// Fix the heap condition by iterating downwards.
  ///
  /// Iterate downwards from the specified entry, checking that its
  /// children satisfy the comparison function.  If they do, stop,
  /// otherwise exchange the parent entry with the highest priority
  /// child.
  ///
  /// This is used when popping items from the queue.
  ///
  /// Parameters:
  ///
  /// - ndx: The index at which to start.
  ///
  private mutating func downHeap(ndx:  Int) {
    var theNdx = ndx
    while true {
      let leftNdx = 2 * theNdx + 1

      if leftNdx >= storage.count {
        break
      }

      let rightNdx = 2 * theNdx + 2
      var largestNdx = theNdx

      if compare(storage[leftNdx], storage[largestNdx]) {
        largestNdx = leftNdx
      }

      if rightNdx < storage.count
           && compare(storage[rightNdx], storage[largestNdx]) {
        largestNdx = rightNdx
      }

      if largestNdx == theNdx {
        break
      }

      storage.swapAt(theNdx, largestNdx)
      theNdx = largestNdx
    }
  }
}

extension PriorityQueue where T: Comparable {
  /// Initialise the priority queue.
  ///
  /// `Comparable` types have a default implementation, which passes the
  /// `>` operator as the comparison function, thus providing a max-queue.
  ///
  /// If you are using a `Comparable` type and require a min-queue, you
  /// can make one with
  ///
  /// ```swift
  /// let minQueue = PriorityQueue(compare: <)
  /// ```
  ///
  init() {
    self.init(compare: >)
  }
}
