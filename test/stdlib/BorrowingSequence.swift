//===--- Iterable.swift --------------------------------------------------===//
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

// RUN: %target-run-stdlib-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults -enable-experimental-feature BorrowingSequence -enable-experimental-feature BorrowingForLoop -enable-experimental-feature Lifetimes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_BorrowingSequence
// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_Lifetimes

import StdlibUnittest

var suite = TestSuite("Iterable Tests")
defer { runAllTests() }

// MARK: - Existing Iterable tests

suite.test("Iterable")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let array = [1, 2, 3, 4, 5, 6, 7, 8]

  let span = array.span
  let spanCollected = span.collectViaBorrowing()
  expectTrue(span.elementsEqual(span))
  expectTrue(spanCollected.elementsEqual(spanCollected))
  expectEqual(array.reduce(0, +), span.reduce(0, +))
  expectEqual(array.reduce(into: 0, +=), span.reduce(into: 0, +=))

  let inline: [8 of Int] = [1, 2, 3, 4, 5, 6, 7, 8]
  let inlineCollected = inline.collectViaBorrowing()
  expectTrue(inline.elementsEqual(inline))
  expectTrue(inlineCollected.elementsEqual(inlineCollected))

  let nocopyInline: [8 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
  let nocopyBuffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 8)
  for i in 0..<8 {
    nocopyBuffer.initializeElement(at: i, to: NoncopyableInt(value: i))
  }
  expectTrue(nocopyInline.elementsEqual(nocopyInline))
}

// MARK: - Iterable protocol tests

suite.test("Iterable/independent-iterators")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span

  var iter1 = span.makeBorrowingIterator()
  var iter2 = span.makeBorrowingIterator()

  // Advance iter1 past first element
  var iterSpan1 = iter1.nextSpan(maxCount: 1)
  expectEqual(iterSpan1.count, 1)
  expectEqual(iterSpan1[0], 1)

  // iter2 is independent and should start from the beginning
  let iterSpan2 = iter2.nextSpan(maxCount: 1)
  expectEqual(iterSpan2.count, 1)
  expectEqual(iterSpan2[0], 1)

  // iter1 should be at the second element
  iterSpan1 = iter1.nextSpan(maxCount: 1)
  expectEqual(iterSpan1.count, 1)
  expectEqual(iterSpan1[0], 2)
}

suite.test("Iterable/collect")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  let collectedEmpty = empty.span.collectViaBorrowing()
  expectTrue(collectedEmpty.isEmpty)

  let arr = [42]
  let collected = arr.span.collectViaBorrowing()
  expectEqual(collected.count, 1)
  expectEqual(collected[0], 42)
}

suite.test("Iterable/elementsEqual")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  expectTrue(empty.span.elementsEqual(empty.span))

  let longer = [1, 2, 3]
  let shorter = [1, 2]
  expectFalse(longer.span.elementsEqual(shorter.span))
  expectFalse(shorter.span.elementsEqual(longer.span))

  let a = [1, 2, 3]
  let b = [1, 2, 4]
  expectFalse(a.span.elementsEqual(b.span))
}

suite.test("Iterable/reduce")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  let result = empty.span.reduce(0, +)
  expectEqual(result, 0)

  let arr = [1, 2, 3, 4, 5]
  let sum = arr.span.reduce(into: 0, +=)
  expectEqual(sum, 15)
}

// MARK: - Span.BorrowingIterator tests

suite.test("Span.BorrowingIterator/basic")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [10, 20, 30, 40, 50]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  var iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 5)
  expectEqual(iterSpan[0], 10)
  expectEqual(iterSpan[4], 50)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

suite.test("Span.BorrowingIterator/partial-reads")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  var iterSpan = iter.nextSpan(maxCount: 2)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0], 1)

  iterSpan = iter.nextSpan(maxCount: 2)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0], 3)

  // Only 1 element left, requesting 2 → get 1
  iterSpan = iter.nextSpan(maxCount: 2)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 5)

  iterSpan = iter.nextSpan(maxCount: 2)
  expectTrue(iterSpan.isEmpty)
}

suite.test("Span.BorrowingIterator/no-arg-convenience")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)
  // nextSpan() with no argument should return the whole remaining span
  let iterSpan = iter.nextSpan()
  expectEqual(iterSpan.count, 3)
  expectEqual(iterSpan[0], 1)
}

suite.test("Span.BorrowingIterator/empty-span")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  let span = empty.span
  var iter = Span.BorrowingIterator(span)
  var iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
  // Repeated calls on an exhausted iterator keep returning empty
  iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

suite.test("Span.BorrowingIterator/single-element")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [99]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)
  var iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 99)
  iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

suite.test("Span.BorrowingIterator/skip-basic")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  let skipped = iter.skip(by: 2)
  expectEqual(skipped, 2)

  let iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 3)
  expectEqual(iterSpan[0], 3)
}

suite.test("Span.BorrowingIterator/skip-past-end")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  // Requesting to skip more than available returns only what existed
  let skipped = iter.skip(by: 100)
  expectEqual(skipped, 3)

  let iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

suite.test("Span.BorrowingIterator/skip-zero")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  let skipped = iter.skip(by: 0)
  expectEqual(skipped, 0)

  let iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 3)
  expectEqual(iterSpan[0], 1)
}

suite.test("Span.BorrowingIterator/skip-negative")
.require(.crashTesting)
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)
  expectCrashLater()
  let skipped = iter.skip(by: -10)
}

suite.test("Span.BorrowingIterator/skip-then-partial-read")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [10, 20, 30, 40, 50, 60]
  let span = arr.span
  var iter = Span.BorrowingIterator(span)

  let skipped = iter.skip(by: 3)
  expectEqual(skipped, 3)

  var iterSpan = iter.nextSpan(maxCount: 2)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0], 40)
  expectEqual(iterSpan[1], 50)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 60)
}

suite.test("Span.BorrowingIterator/noncopyable-elements")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let buffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
  for i in 0..<4 {
    buffer.initializeElement(at: i, to: NoncopyableInt(value: i * 10))
  }
  let span = Span<NoncopyableInt>(_unsafeElements: buffer)

  // Skip 2, then read remaining 2
  var iter = Span.BorrowingIterator(span)
  let skipped = iter.skip(by: 2)
  expectEqual(skipped, 2)

  var iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0].value, 20)
  expectEqual(iterSpan[1].value, 30)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

// MARK: - BorrowingIteratorAdapter tests

suite.test("BorrowingIteratorAdapter/basic")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [10, 20, 30]
  var iter = BorrowingIteratorAdapter(iterator: arr.makeIterator())

  var collected: [Int] = []
  while true {
    let span = iter.nextSpan(maxCount: .max)
    if span.isEmpty { break }
    for i in span.indices { collected.append(span[i]) }
  }
  expectEqual(collected, [10, 20, 30])
}

suite.test("BorrowingIteratorAdapter/one-element-per-call")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  // BorrowingIteratorAdapter buffers one element at a time via Optional<T>
  let arr = [100, 200, 300]
  var iter = BorrowingIteratorAdapter(iterator: arr.makeIterator())

  var iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 100)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 200)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 1)
  expectEqual(iterSpan[0], 300)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectTrue(iterSpan.isEmpty)
}

suite.test("BorrowingIteratorAdapter/empty")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  var iter = BorrowingIteratorAdapter(iterator: empty.makeIterator())
  let span = iter.nextSpan(maxCount: .max)
  expectTrue(span.isEmpty)
}

suite.test("BorrowingIteratorAdapter/skip-via-default-implementation")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  // BorrowingIteratorAdapter uses the default skip(by:) from
  // BorrowingIteratorProtocol, which loops calling nextSpan.
  let arr = [10, 20, 30, 40, 50]
  var iter = BorrowingIteratorAdapter(iterator: arr.makeIterator())

  let skipped = iter.skip(by: 3)
  expectEqual(skipped, 3)

  let next = iter.nextSpan(maxCount: .max)
  expectEqual(next.count, 1)
  expectEqual(next[0], 40)
}

suite.test("BorrowingIteratorAdapter/skip-past-end")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [10, 20, 30]
  var iter = BorrowingIteratorAdapter(iterator: arr.makeIterator())

  let skipped = iter.skip(by: 100)
  expectEqual(skipped, 3)

  let done = iter.nextSpan(maxCount: .max)
  expectTrue(done.isEmpty)
}

// MARK: - InlineArray Iterable tests

suite.test("InlineArray/collect-via-iterable")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let inline: [4 of Int] = [10, 20, 30, 40]
  let collected = inline.collectViaBorrowing()
  expectEqual(collected, [10, 20, 30, 40])
}

suite.test("InlineArray/makeBorrowingIterator-partial")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let inline: [5 of Int] = [1, 2, 3, 4, 5]
  var iter = inline.makeBorrowingIterator()

  var iterSpan = iter.nextSpan(maxCount: 3)
  expectEqual(iterSpan.count, 3)
  expectEqual(iterSpan[0], 1)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0], 4)
}

suite.test("InlineArray/skip")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let inline: [6 of Int] = [10, 20, 30, 40, 50, 60]
  var iter = inline.makeBorrowingIterator()

  let skipped = iter.skip(by: 4)
  expectEqual(skipped, 4)

  let remaining = iter.nextSpan(maxCount: .max)
  expectEqual(remaining.count, 2)
  expectEqual(remaining[0], 50)
  expectEqual(remaining[1], 60)
}

suite.test("InlineArray/noncopyable-elementsEqual")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let a: [4 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
  let b: [4 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
  expectTrue(a.elementsEqual(b))
  expectTrue(a.elementsEqual(a))
}

// MARK: - UniqueArray Iterable tests

suite.test("UniqueArray/collect-via-iterable")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let unique = UniqueArray(copying: 1...5)
  let collected = unique.collectViaBorrowing()
  expectEqual(collected, [1, 2, 3, 4, 5])
}

suite.test("UniqueArray/makeBorrowingIterator-partial")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let unique = UniqueArray(copying: 1...5)
  var iter = unique.makeBorrowingIterator()

  var iterSpan = iter.nextSpan(maxCount: 3)
  expectEqual(iterSpan.count, 3)
  expectEqual(iterSpan[0], 1)

  iterSpan = iter.nextSpan(maxCount: .max)
  expectEqual(iterSpan.count, 2)
  expectEqual(iterSpan[0], 4)
}

suite.test("UniqueArray/skip")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let unique = UniqueArray(copying: 1...5)
  var iter = unique.makeBorrowingIterator()

  let skipped = iter.skip(by: 3)
  expectEqual(skipped, 3)

  let remaining = iter.nextSpan(maxCount: .max)
  expectEqual(remaining.count, 2)
  expectEqual(remaining[0], 4)
  expectEqual(remaining[1], 5)
}

suite.test("UniqueArray/noncopyable-elementsEqual")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let a: [4 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
  let b = UniqueArray<NoncopyableInt>(capacity: 4) { outputSpan in
    for i in 0..<4 {
      outputSpan.append(NoncopyableInt(value: i))
    }
  }
  expectTrue(a.elementsEqual(b))
  expectTrue(b.elementsEqual(a))
  expectTrue(b.elementsEqual(b))
}

// MARK: - Throwing Iterable tests

#if false // error: lifetime-dependent value escapes its scope

suite.test("ThrowingIterable/success-no-throw")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  // limit > count: inner span exhausts before limit is reached, no throw
  let seq = LimitedSeq(span, limit: 10)
  do {
    let collected = try seq.collectViaBorrowing()
    expectEqual(collected, [1, 2, 3, 4, 5])
  } catch {
    expectTrue(false)
  }
}

suite.test("ThrowingIterable/throws-at-limit")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  // limit = 3: throws after yielding 3 elements
  let seq = LimitedSeq(span, limit: 3)
  do {
    let _ = try seq.collectViaBorrowing()
    expectTrue(false) // expected throw
  } catch CountdownError.limitReached {
    // correct
  } catch {
    expectTrue(false) // unexpected error type
  }
}

suite.test("ThrowingIterable/throws-immediately")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  // limit = 0: throws on first call to nextSpan
  let seq = LimitedSeq(span, limit: 0)
  do {
    let _ = try seq.collectViaBorrowing()
    expectTrue(false) // expected throw
  } catch CountdownError.limitReached {
    // correct
  } catch {
    expectTrue(false) // unexpected error type
  }
}

suite.test("ThrowingIterable/exact-limit-no-throw")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  // limit == count: iterates exactly the whole span without throwing
  // because the inner Span.BorrowingIterator exhausts before the limit check fires
  let arr = [1, 2, 3]
  let span = arr.span
  let seq = LimitedSeq(span, limit: arr.count)
  do {
    let collected = try seq.collectViaBorrowing()
    expectEqual(collected, [1, 2, 3])
  } catch {
    expectTrue(false)
  }
}
#endif

#if false // error: lifetime-dependent variable '$generator' escapes its scope

// MARK: - for-in loop tests

suite.test("ForIn/basic-span")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [10, 20, 30, 40, 50]
  var collected: [Int] = []
  for element in arr.span {
    collected.append(element)
  }
  expectEqual(collected, arr)
}

suite.test("ForIn/break")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  var collected: [Int] = []
  for element in arr.span {
    if element == 3 { break }
    collected.append(element)
  }
  expectEqual(collected, [1, 2])
}

suite.test("ForIn/continue")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  var collected: [Int] = []
  for element in arr.span {
    if element.isMultiple(of: 2) { continue }
    collected.append(element)
  }
  expectEqual(collected, [1, 3, 5])
}

suite.test("ForIn/where-clause")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5, 6]
  var collected: [Int] = []
  for element in arr.span where element.isMultiple(of: 2) {
    collected.append(element)
  }
  expectEqual(collected, [2, 4, 6])
}

suite.test("ForIn/empty-span")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let empty: [Int] = []
  var count = 0
  for _ in empty.span {
    count += 1
  }
  expectEqual(count, 0)
}

suite.test("ForIn/noncopyable-elements")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let buffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
  for i in 0..<4 {
    buffer.initializeElement(at: i, to: NoncopyableInt(value: i * 10))
  }
  let span = Span<NoncopyableInt>(_unsafeElements: buffer)

  var sum = 0
  for element in span {
    sum += element.value
  }
  expectEqual(sum, 60)
}

suite.test("ForIn/inline-array")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let inline: [5 of Int] = [2, 4, 6, 8, 10]
  var sum = 0
  for element in inline {
    sum += element
  }
  expectEqual(sum, 30)
}

suite.test("ForIn/nested-loops")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let outer = [1, 2, 3]
  let inner = [10, 20]
  var pairs: [(Int, Int)] = []
  for x in outer.span {
    for y in inner.span {
      pairs.append((x, y))
    }
  }
  expectEqual(pairs.count, 6)
  expectEqual(pairs[0].0, 1); expectEqual(pairs[0].1, 10)
  expectEqual(pairs[1].0, 1); expectEqual(pairs[1].1, 20)
  expectEqual(pairs[5].0, 3); expectEqual(pairs[5].1, 20)
}

// MARK: - for try-in loop tests

suite.test("ForTryIn/success-no-throw")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  let seq = LimitedSeq(span, limit: 10)  // limit > count, no throw

  var collected: [Int] = []
  do {
    for try element in seq {
      collected.append(element)
    }
  } catch {
    expectTrue(false)
  }
  expectEqual(collected, [1, 2, 3, 4, 5])
}

suite.test("ForTryIn/throws-partway-through")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  let seq = LimitedSeq(span, limit: 2)  // throws after yielding 2 elements

  var collectedBeforeThrow: [Int] = []
  var caughtError: CountdownError? = nil
  do {
    for try element in seq {
      collectedBeforeThrow.append(element)
    }
  } catch let e as CountdownError {
    caughtError = e
  } catch {
    expectTrue(false)
  }
  expectEqual(collectedBeforeThrow, [1, 2])
  expectEqual(caughtError, .limitReached)
}

suite.test("ForTryIn/throws-immediately")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3]
  let span = arr.span
  let seq = LimitedSeq(span, limit: 0)  // throws before yielding anything

  var bodyExecuted = false
  do {
    for try element in seq {
      bodyExecuted = true
      _ = element
    }
    expectTrue(false) // expected throw
  } catch CountdownError.limitReached {
    // correct
  } catch {
    expectTrue(false)
  }
  expectFalse(bodyExecuted)
}

suite.test("ForTryIn/break-before-throw")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  let seq = LimitedSeq(span, limit: 3)  // would throw after 3, but we break at 2

  do {
    for try element in seq {
      if element == 2 { break }
      _ = element
    }
    // break exits cleanly without throwing
  } catch {
    expectTrue(false)
  }
}

suite.test("ForTryIn/continue-in-throwing-loop")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span
  let seq = LimitedSeq(span, limit: 10)  // no throw

  var collected: [Int] = []
  do {
    for try element in seq {
      if element.isMultiple(of: 2) { continue }
      collected.append(element)
    }
  } catch {
    expectTrue(false)
  }
  expectEqual(collected, [1, 3, 5])
}

suite.test("ForTryIn/rethrow")
.require(.stdlib_6_4).code {
  guard #available(SwiftStdlib 6.4, *) else { return }

  // Helper that iterates and rethrows, proving typed errors propagate
  @available(SwiftStdlib 6.4, *)
  func sumThrowing(_ seq: borrowing LimitedSeq) throws(CountdownError) -> Int {
    var total = 0
    for try element in seq {
      total += element
    }
    return total
  }

  let arr = [1, 2, 3, 4, 5]
  let span = arr.span

  // No throw: sum of all elements
  let okSeq = LimitedSeq(span, limit: 10)
  let okSum = try? sumThrowing(okSeq)
  expectEqual(okSum, 15)

  // Throws: partial sum
  let throwSeq = LimitedSeq(span, limit: 2)
  do {
    let _ = try sumThrowing(throwSeq)
    expectTrue(false)
  } catch CountdownError.limitReached {
    // correct
  } catch {
    expectTrue(false)
  }
}

#endif

// MARK: - Helpers

struct NoncopyableInt: ~Copyable, Equatable {
  var value: Int

  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 6.4, *)
extension Iterable where Self: ~Copyable & ~Escapable, Element: ~Copyable, Failure == Never {
  func reduce<T: ~Copyable, E: Error>(
    _ initial: consuming T,
    _ nextPartialResult: @escaping (consuming T, borrowing Element) throws(E) -> T
  ) throws(E) -> T {
    var borrowIterator = makeBorrowingIterator()
    var result = initial
    while true {
      let span = borrowIterator.nextSpan(maxCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        result = try nextPartialResult(result, span[i])
      }
    }
    return result
  }

  func reduce<T: ~Copyable, E: Error>(
    into initial: consuming T,
    _ nextPartialResult: (inout T, borrowing Element) throws(E) -> Void
  ) throws(E) -> T {
    var borrowIterator = makeBorrowingIterator()
    var result = initial
    while true {
      let span = borrowIterator.nextSpan(maxCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        try nextPartialResult(&result, span[i])
      }
    }
    return result
  }
}

@available(SwiftStdlib 6.4, *)
extension Iterable where Self: ~Copyable & ~Escapable, Element: Copyable {
  func collectViaBorrowing() throws(Failure) -> [Element] {
    var borrowIterator = makeBorrowingIterator()
    var result: [Element] = []
    while true {
      let span = try borrowIterator.nextSpan(maxCount: .max)
      if span.isEmpty { break }
      for i in span.indices {
        result.append(span[i])
      }
    }
    return result
  }
}

@available(SwiftStdlib 6.4, *)
extension Iterable where Self: ~Escapable & ~Copyable, Element: Equatable & ~Copyable {
  func elementsEqual<S: Iterable<Element, Failure>>(
    _ rhs: borrowing S
  ) throws(Failure) -> Bool
    where S: ~Escapable & ~Copyable, S.Element: ~Copyable, S.Failure == Failure
  {
    var iter1 = makeBorrowingIterator()
    var iter2 = rhs.makeBorrowingIterator()
    while true {
      var el1 = try iter1.nextSpan(maxCount: .max)

      if el1.isEmpty {
        let el2 = try iter2.nextSpan(maxCount: 1)
        return el2.isEmpty
      }

      while el1.count > 0 {
        let el2 = try iter2.nextSpan(maxCount: el1.count)
        if el2.isEmpty { return false }
        for i in 0..<el2.count {
          if el1[i] != el2[i] { return false }
        }
        el1 = el1.extracting(droppingFirst: el2.count)
      }
    }
  }
}

// MARK: - Throwing sequence helpers

enum CountdownError: Error, Equatable {
  case limitReached
}

@available(SwiftStdlib 6.4, *)
struct LimitedIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = CountdownError

  var _inner: Span<Int>.BorrowingIterator
  var _remaining: Int

  @_lifetime(copy inner)
  init(_ inner: consuming Span<Int>.BorrowingIterator, limit: Int) {
    _inner = inner
    _remaining = limit
  }

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(CountdownError) -> Span<Int> {
    guard _remaining > 0 else { throw .limitReached }
    let n = Swift.min(maxCount, _remaining)
    let span = _inner.nextSpan(maxCount: n)
    _remaining -= span.count
    return span
  }
}

@available(SwiftStdlib 6.4, *)
struct LimitedSeq: Iterable, ~Copyable, ~Escapable {
  var _span: Span<Int>
  var _limit: Int

  @_lifetime(copy span)
  init(_ span: Span<Int>, limit: Int) {
    _span = span
    _limit = limit
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> LimitedIterator {
    LimitedIterator(_span.makeBorrowingIterator(), limit: _limit)
  }
}
