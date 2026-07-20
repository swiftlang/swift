// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes \
// RUN: -Xfrontend -disable-availability-checking) \
// RUN: %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

enum IterationError: Error, CustomStringConvertible {
  case reachedLimit(Int)
  case corrupted

  var description: String {
    switch self {
    case .reachedLimit(let n): return "reachedLimit(\(n))"
    case .corrupted: return "corrupted"
    }
  }
}

// A throwing BorrowingIteratorProtocol that throws after yielding a certain
// number of elements.
@available(SwiftStdlib 6.4, *)
struct LimitedThrowingIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError

  var _inner: Span<Int>.BorrowingIterator
  var _remaining: Int

  @_lifetime(copy inner)
  init(_inner inner: consuming Span<Int>.BorrowingIterator, _remaining remaining: Int) {
    self._inner = inner
    self._remaining = remaining
  }

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(IterationError) -> Span<Int> {
    if _remaining <= 0 {
      throw .reachedLimit(_remaining)
    }
    let span = _inner.nextSpan(maxCount: Swift.min(maxCount, _remaining))
    _remaining -= span.count
    return span
  }
}

@available(SwiftStdlib 6.4, *)
struct LimitedThrowingSeq: Iterable, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError
  typealias BorrowingIterator = LimitedThrowingIterator

  var _span: Span<Int>
  var _limit: Int

  @_lifetime(copy span)
  init(_span span: Span<Int>, _limit limit: Int) {
    self._span = span
    self._limit = limit
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> LimitedThrowingIterator {
    LimitedThrowingIterator(_inner: .init(_span), _remaining: _limit)
  }
}

// An iterator that always throws immediately.
@available(SwiftStdlib 6.4, *)
struct ImmediatelyThrowingIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError

  @_lifetime(immortal)
  init() {}

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(IterationError) -> Span<Int> {
    throw .corrupted
  }
}

@available(SwiftStdlib 6.4, *)
struct ImmediatelyThrowingSeq: Iterable, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError
  typealias BorrowingIterator = ImmediatelyThrowingIterator

  var _span: Span<Int>

  @_lifetime(copy span)
  init(_span span: Span<Int>) {
    self._span = span
  }

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> ImmediatelyThrowingIterator {
    ImmediatelyThrowingIterator()
  }
}

@available(SwiftStdlib 6.4, *)
func testThrowAfterElements(seq: borrowing LimitedThrowingSeq) {
  do {
    for try element in seq {
      // CHECK: element = 0
      // CHECK: element = 1
      print("element = \(element)")
    }
  } catch {
    // CHECK: caught: reachedLimit(0)
    print("caught: \(error)")
  }
}

@available(SwiftStdlib 6.4, *)
func testNoThrowWhenUnderLimit(seq: borrowing LimitedThrowingSeq) {
  do {
    for try element in seq {
      // CHECK: ok = 0
      // CHECK: ok = 1
      // CHECK: ok = 2
      // CHECK: ok = 3
      print("ok = \(element)")
    }
    // CHECK: completed without error
    print("completed without error")
  } catch {
    print("unexpected: \(error)")
  }
}

@available(SwiftStdlib 6.4, *)
func testImmediateThrow(seq: borrowing ImmediatelyThrowingSeq) {
  var count = 0
  do {
    for try element in seq {
      count += 1
      _ = element
    }
  } catch {
    // CHECK: immediate error: corrupted, count: 0
    print("immediate error: \(error), count: \(count)")
  }
}

@available(SwiftStdlib 6.4, *)
func iterateAndRethrow(seq: borrowing LimitedThrowingSeq) throws(IterationError) -> Int {
  var sum = 0
  for try element in seq {
    sum += element
  }
  return sum
}

@available(SwiftStdlib 6.4, *)
func testRethrow(seq: borrowing LimitedThrowingSeq) {
  do {
    let result = try iterateAndRethrow(seq: seq)
    print("sum = \(result)")
  } catch {
    // CHECK: rethrown: reachedLimit(0)
    print("rethrown: \(error)")
  }
}

@available(SwiftStdlib 6.4, *)
func testBreakBeforeThrow(seq: borrowing LimitedThrowingSeq) {
  do {
    for try element in seq {
      if element == 1 {
        break
      }
      // CHECK: before-break = 0
      print("before-break = \(element)")
    }
    // CHECK: break exited cleanly
    print("break exited cleanly")
  } catch {
    print("unexpected error: \(error)")
  }
}

@available(SwiftStdlib 6.4, *)
func testContinueInThrowingLoop(seq: borrowing LimitedThrowingSeq) {
  do {
    for try element in seq {
      if element == 1 {
        continue
      }
      // CHECK: not-skipped = 0
      // CHECK: not-skipped = 2
      // CHECK: not-skipped = 3
      print("not-skipped = \(element)")
    }
    // CHECK: continue loop done
    print("continue loop done")
  } catch {
    print("unexpected: \(error)")
  }
}

@available(SwiftStdlib 6.4, *)
func testNestedThrowingLoops(outer: borrowing LimitedThrowingSeq, inner: borrowing Span<Int>) {
  do {
    for try element in outer {
      var innerSum = 0
      for x in inner {
        innerSum += x
      }
      // CHECK: outer=0, innerSum=6
      // CHECK: outer=1, innerSum=6
      print("outer=\(element), innerSum=\(innerSum)")
    }
  } catch {
    // CHECK: nested caught: reachedLimit(0)
    print("nested caught: \(error)")
  }
}

// Entry point
let arr = [0, 1, 2, 3]
let innerArr = [1, 2, 3]

if #available(SwiftStdlib 6.4, *) {
  let span = arr.span
  let innerSpan = innerArr.span

  // Limit of 2: will throw after yielding elements 0 and 1
  let throwingSeq = LimitedThrowingSeq(_span: span, _limit: 2)
  testThrowAfterElements(seq: throwingSeq)

  // Limit of 10: more than enough, no throw
  let nonThrowingSeq = LimitedThrowingSeq(_span: span, _limit: 10)
  testNoThrowWhenUnderLimit(seq: nonThrowingSeq)

  // Immediate throw
  let immediateSeq = ImmediatelyThrowingSeq(_span: span)
  testImmediateThrow(seq: immediateSeq)

  // Rethrow test with limit 2
  let rethrowSeq = LimitedThrowingSeq(_span: span, _limit: 2)
  testRethrow(seq: rethrowSeq)

  // Break before the throw triggers (limit=3, break at element 1)
  let breakSeq = LimitedThrowingSeq(_span: span, _limit: 3)
  testBreakBeforeThrow(seq: breakSeq)

  // Continue with limit high enough to not throw
  let continueSeq = LimitedThrowingSeq(_span: span, _limit: 10)
  testContinueInThrowingLoop(seq: continueSeq)

  // Nested loops: outer throws after 2 elements
  let nestedOuterSeq = LimitedThrowingSeq(_span: span, _limit: 2)
  testNestedThrowingLoops(outer: nestedOuterSeq, inner: innerSpan)
}
