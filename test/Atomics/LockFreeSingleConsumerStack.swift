// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// FIXME Run with TSAN

import StdlibUnittest
import Atomics
import Dispatch

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
class LockFreeSingleConsumerStack<Element> {
  struct Node {
    let value: Element
    var next: UnsafeMutablePointer<Node>?
  }
  typealias NodePtr = UnsafeMutablePointer<Node>

  private var _last = UnsafeAtomic<NodePtr?>.create(initialValue: nil)
  private var _consumerCount = UnsafeAtomic<Int>.create(initialValue: 0)

  deinit {
    // Discard remaining nodes
    while let _ = pop() {}
    _last.destroy()
    _consumerCount.destroy()
  }

  // Push the given element to the top of the stack.
  // It is okay to concurrently call this in an arbitrary number of threads.
  func push(_ value: Element) {
    let new = NodePtr.allocate(capacity: 1)
    new.initialize(to: Node(value: value, next: nil))

    var done = false
    var current = _last.load(ordering: .relaxed)
    while !done {
      new.pointee.next = current
      (done, current) = _last.compareExchange(
        expected: current,
        desired: new,
        ordering: .releasing)
    }
  }

  // Pop and return the topmost element from the stack.
  // This method does not support multiple overlapping concurrent calls.
  func pop() -> Element? {
    precondition(
      _consumerCount.loadThenWrappingIncrement(ordering: .acquiring) == 0,
      "Multiple consumers detected")
    defer { _consumerCount.wrappingDecrement(ordering: .releasing) }
    var done = false
    var current = _last.load(ordering: .acquiring)
    while let c = current {
      (done, current) = _last.compareExchange(
        expected: c,
        desired: c.pointee.next,
        ordering: .acquiring)
      if done {
        let result = c.move()
        c.deallocate()
        return result.value
      }
    }
    return nil
  }
}

let suite = TestSuite("LockFreeSingleConsumerStack")
defer { runAllTests() }

suite.test("Basics") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let stack = LockFreeSingleConsumerStack<Int>()
  expectNil(stack.pop())
  stack.push(0)
  expectEqual(0, stack.pop())

  stack.push(1)
  stack.push(2)
  stack.push(3)
  stack.push(4)
  expectEqual(4, stack.pop())
  expectEqual(3, stack.pop())
  expectEqual(2, stack.pop())
  expectEqual(1, stack.pop())
  expectNil(stack.pop())
}

suite.test("ConcurrentPushes") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let stack = LockFreeSingleConsumerStack<(thread: Int, value: Int)>()

  let numThreads = 100
  let numValues = 10_000
  DispatchQueue.concurrentPerform(iterations: numThreads) { thread in
    for value in 1 ... numValues {
      stack.push((thread: thread, value: value))
    }
  }

  var expected: [Int] = Array(repeating: numValues, count: numThreads)
  while let (thread, value) = stack.pop() {
    expectEqual(expected[thread], value)
    expected[thread] -= 1
  }
  expectEqual(Array(repeating: 0, count: numThreads), expected)
}

suite.test("ConcurrentPushesAndPops") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let stack = LockFreeSingleConsumerStack<(thread: Int, value: Int)>()

  let numThreads = 100
  let numValues = 10_000

  var perThreadSums: [Int] = Array(repeating: 0, count: numThreads)
  let consumerQueue = DispatchQueue(label: "org.swift.background")
  consumerQueue.async {
    var count = 0
    while count < numThreads * numValues {
      // Note: busy wait
      if let (thread, value) = stack.pop() {
        perThreadSums[thread] += value
        count += 1
      }
    }
  }

  DispatchQueue.concurrentPerform(iterations: numThreads + 1) { thread in
    if thread < numThreads {
      // Producers
      for value in 0 ..< numValues {
        stack.push((thread: thread, value: value))
      }
    }
  }

  consumerQueue.sync {
    expectEqual(Array(repeating: numValues * (numValues - 1) / 2, count: numThreads), perThreadSums)
  }
}
