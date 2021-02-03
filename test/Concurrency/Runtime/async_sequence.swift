// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency)

// REQUIRES: executable_test
// REQUIRES: concurrency

import StdlibUnittest

// Utility functions for closure based operators to force them into throwing 
// and async and throwing async contexts.

func throwing<T>(_ value: T) throws -> T {
  return value
}

func asynchronous<T>(_ value: T) async -> T {
  return value
}

func asynchronousThrowing<T>(_ value: T) async throws -> T {
  return value
}

struct Failure: Error { }

func failable<T, E: Error>(
  _ results: [Result<T, E>]
) -> AsyncFailableMapSequence<AsyncLazySequence<[Result<T, E>]>, T> {
  return results.async.map { try $0.get() }
}


extension Sequence {
  @inlinable
  public var async: AsyncLazySequence<Self> {
    get {
      return AsyncLazySequence(self)
    }
  }
}

@frozen
public struct AsyncLazySequence<S: Sequence>: AsyncSequence {
  public typealias Element = S.Element
  public typealias AsyncIterator = Iterator

  @frozen
  public struct Iterator: AsyncIteratorProtocol {
    @usableFromInline
    var iterator: S.Iterator

    @usableFromInline
    init(_ iterator: S.Iterator) {
      self.iterator = iterator
    }

    @inlinable
    public mutating func next() async -> S.Element? {
      return iterator.next()
    }
  }

  public let sequence: S

  @inlinable
  public init(_ sequence: S) {
    self.sequence = sequence
  }

  @inlinable
  public __consuming func makeAsyncIterator() -> Iterator {
    return Iterator(sequence.makeIterator())
  }
}

extension AsyncSequence {
  @inlinable
  public func collect() async rethrows -> [Element] {
    var items = [Element]()
    var it = makeAsyncIterator()
    while let e = try await it.next() {
      items.append(e)
    }
    return items
  }
}

var AsyncLazySequenceTests = TestSuite("AsyncLazySequence")

AsyncLazySequenceTests.test("iteration past first nil") {
  runAsyncAndBlock {
    let seq = [1, 2, 3]
    let async_seq = seq.async
    var iter = async_seq.makeAsyncIterator()
    let a = await iter.next()
    expectEqual(a, 1)
    let b = await iter.next()
    expectEqual(b, 2)
    let c = await iter.next()
    expectEqual(c, 3)
    let d = await iter.next()
    expectNil(d)
    let e = await iter.next()
    expectNil(e)
  }
}

AsyncLazySequenceTests.test("iterate twice with iterators before usage") {
  runAsyncAndBlock {
    let s = [1, 2, 3].async
    var iter1 = s.makeAsyncIterator()
    var iter2 = s.makeAsyncIterator()
    var iter1Results = [Int?]()
    var iter2Results = [Int?]()
    for _ in 1...4 {
      do {
        let value = await iter1.next()
        iter1Results.append(value)
      }

      do {
        let value = await iter2.next()
        iter2Results.append(value)
      }
    }
    expectEqual(iter1Results, iter2Results)
    expectEqual(iter1Results, [1, 2, 3, nil])
  }
}

var AsyncSequenceTests = TestSuite("AsyncSequence")

AsyncSequenceTests.test("reduce") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.reduce(0) { partial, element in
      return partial + element
    }
    expectEqual(result, 1 + 2 + 3)
  }
}

AsyncSequenceTests.test("reduce into") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.reduce(into: 0) { partial, element in
      partial += element
    }
    expectEqual(result, 1 + 2 + 3)
  }
}

AsyncSequenceTests.test("contains predicate") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.contains { $0 == 2 }
    expectTrue(result)
  }

  runAsyncAndBlock {
    let result = await [1, 2, 3].async.contains { $0 == 4 }
    expectFalse(result)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.contains { $0 == 4 }
    expectFalse(result)
  }
}

AsyncSequenceTests.test("all satisfy") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.allSatisfy { $0 < 10 }
    expectTrue(result)
  }

  runAsyncAndBlock {
    let result = await [1, 2, 3].async.allSatisfy { $0 > 10 }
    expectFalse(result)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.allSatisfy { $0 > 10 }
    expectTrue(result)
  }
}

AsyncSequenceTests.test("contains") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.contains(2)
    expectTrue(result)
  }

  runAsyncAndBlock {
    let result = await [1, 2, 3].async.contains(4)
    expectFalse(result)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.contains(4)
    expectFalse(result)
  }
}

AsyncSequenceTests.test("first") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.first { $0 > 1 }
    expectEqual(result, 2)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.first { $0 > 1 }
    expectEqual(result, nil)
  }
}

AsyncSequenceTests.test("min by") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.min { $0 < $1}
    expectEqual(result, 1)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.min { $0 < $1}
    expectEqual(result, nil)
  }
}

AsyncSequenceTests.test("max by") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.max { $0 < $1}
    expectEqual(result, 3)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.max { $0 < $1}
    expectEqual(result, nil)
  }
}

AsyncSequenceTests.test("min") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.min()
    expectEqual(result, 1)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.min()
    expectEqual(result, nil)
  }
}

AsyncSequenceTests.test("max") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.max()
    expectEqual(result, 3)
  }

  runAsyncAndBlock {
    let result = await [Int]().async.max()
    expectEqual(result, nil)
  }
}

AsyncSequenceTests.test("collect") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.collect()
    expectEqual(result, [1, 2, 3])
  }

  runAsyncAndBlock {
    let result = await [Int]().async.collect()
    expectEqual(result, [])
  }
}

var AsyncCompactMapTests = TestSuite("AsyncCompactMap")

AsyncCompactMapTests.test("non nil values") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async.compactMap { $0 }.collect()
    expectEqual(result, [1, 2, 3])
  }
}

AsyncCompactMapTests.test("nil values") {
  runAsyncAndBlock {
    let result = await [1, nil, 2, nil, 3, nil].async
      .compactMap { $0 }.collect()
    expectEqual(result, [1, 2, 3])
  }
}

AsyncCompactMapTests.test("non nil values async") {
  runAsyncAndBlock {
    let result = await [1, 2, 3].async
      .compactMap { await asynchronous($0) }.collect()
    expectEqual(result, [1, 2, 3])
  }
}

AsyncCompactMapTests.test("nil values async") {
  runAsyncAndBlock {
    let result = await [1, nil, 2, nil, 3, nil].async
      .compactMap { await asynchronous($0) }.collect()
    expectEqual(result, [1, 2, 3])
  }
}

AsyncCompactMapTests.test("non nil values with throw") {
  runAsyncAndBlock {
    do {
      let result = try await [1, 2, 3].async
        .compactMap { try throwing($0) }.collect()
      expectEqual(result, [1, 2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncCompactMapTests.test("nil values with throw") {
  runAsyncAndBlock {
    do {
      let result = try await [1, nil, 2, nil, 3, nil].async
        .compactMap { try throwing($0) }.collect()
      expectEqual(result, [1, 2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncCompactMapTests.test("non nil values with async throw") {
  runAsyncAndBlock {
    do {
      let result = try await [1, 2, 3].async
        .compactMap { try await asynchronousThrowing($0) }.collect()
      expectEqual(result, [1, 2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncCompactMapTests.test("nil values with async throw") {
  runAsyncAndBlock {
    do {
      let result = try await [1, nil, 2, nil, 3, nil].async
        .compactMap { try await asynchronousThrowing($0) }.collect()
      expectEqual(result, [1, 2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncCompactMapTests.test("throwing") {
  runAsyncAndBlock {
    do {
      _ = try await [1, nil, 2, nil, 3, nil].async
        .compactMap { value throws -> Int? in
          if value == 2 {
            throw Failure()
          }
          return value
        }
        .collect()
      expectUnreachable()
    } catch {

    }
  }
}

var AsyncMapSequenceTests = TestSuite("AsyncMapSequence")

AsyncMapSequenceTests.test("map values") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.map { "\($0)" }.collect()
    expectEqual(results, ["1", "2", "3"])
  }
}

AsyncMapSequenceTests.test("map empty") {
  runAsyncAndBlock {
    let results = await [Int]().async.map { "\($0)" }.collect()
    expectEqual(results, [])
  }
}

AsyncMapSequenceTests.test("map throwing") {
  runAsyncAndBlock {
    let seq = [1, 2, 3].async
      .map { value throws -> String in
        if value == 2 {
          throw Failure()
        }
        return "\(value)" 
      }
    var it = seq.makeAsyncIterator()
    var results = [String]()
    do {
      while let value = try await it.next() {
        results.append(value)
      }
      expectUnreachable()
    } catch {
      expectEqual(results, ["1"])
    }
  }
}

var AsyncFilterSequenceTests = TestSuite("AsyncFilterSequence")

AsyncFilterSequenceTests.test("filter out one item") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.filter { $0 % 2 != 0 } .collect()
    expectEqual(results, [1, 3])
  }
}

AsyncFilterSequenceTests.test("filter out one item throwing") {
  runAsyncAndBlock {
    do {
      let results = try await [1, 2, 3].async
        .filter { try throwing($0 % 2 != 0) }
        .collect()
      expectEqual(results, [1, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncFilterSequenceTests.test("filter out one item throwing async") {
  runAsyncAndBlock {
    do {
      let results = try await [1, 2, 3].async
        .filter { try await asynchronousThrowing($0 % 2 != 0) }
        .collect()
      expectEqual(results, [1, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncFilterSequenceTests.test("filter out two items") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.filter { $0 % 2 == 0 } .collect()
    expectEqual(results, [2])
  }
}

AsyncFilterSequenceTests.test("filter out all items") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.filter { _ in return false } .collect()
    expectEqual(results, [Int]())
  }
}

AsyncFilterSequenceTests.test("filter out no items") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.filter { _ in return true } .collect()
    expectEqual(results, [1, 2, 3])
  }
}

var AsyncDropWhileSequenceTests = TestSuite("AsyncDropWhileSequence")

AsyncDropWhileSequenceTests.test("drop one") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.drop { value -> Bool in
      return value == 1
    }.collect()
    expectEqual(results, [2, 3])
  }
}

AsyncDropWhileSequenceTests.test("drop one async") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.drop { value async -> Bool in
      return await asynchronous(value == 1)
    }.collect()
    expectEqual(results, [2, 3])
  }
}

AsyncDropWhileSequenceTests.test("drop one throws") {
  runAsyncAndBlock {
    do {
      let results = try await [1, 2, 3].async.drop { value throws -> Bool in
        return try throwing(value == 1)
      }.collect()
      expectEqual(results, [2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncDropWhileSequenceTests.test("drop one throws async") {
  runAsyncAndBlock {
    do {
      let results = try await [1, 2, 3].async.drop { value async throws -> Bool in
        return try await asynchronousThrowing(value == 1)
      }.collect()
      expectEqual(results, [2, 3])
    } catch {
      expectUnreachable()
    }
  }
}

AsyncDropWhileSequenceTests.test("drop none") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.drop { _ in return false }.collect()
    expectEqual(results, [1, 2, 3])
  }
}

AsyncDropWhileSequenceTests.test("drop all") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.drop { _ in return true }.collect()
    expectEqual(results, [])
  }
}

AsyncDropWhileSequenceTests.test("drop stops") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.drop { value in
      return value == 1 || value == 3
    }.collect()
    expectEqual(results, [2, 3])
  }
}

var AsyncDropFirstSequenceTests = TestSuite("AsyncDropFirstSequence")

AsyncDropFirstSequenceTests.test("drop 1") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.dropFirst().collect()
    expectEqual(results, [2, 3])
  }
}

AsyncDropFirstSequenceTests.test("drop 2") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.dropFirst(2).collect()
    expectEqual(results, [3])
  }
}

AsyncDropFirstSequenceTests.test("drop 0") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.dropFirst(0).collect()
    expectEqual(results, [1, 2, 3])
  }
}

var AsyncPrefixSequenceTests = TestSuite("AsyncPrefixSequence")

AsyncPrefixSequenceTests.test("prefix satisfied") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.prefix(2).collect()
    expectEqual(results, [1, 2])
  }
}

AsyncPrefixSequenceTests.test("prefix partial satisfied") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.prefix(4).collect()
    expectEqual(results, [1, 2, 3])
  }
}

AsyncPrefixSequenceTests.test("prefix 0") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.prefix(0).collect()
    expectEqual(results, [])
  }
}

var AsyncPrefixWhileSequenceTests = TestSuite("AsyncPrefixWhileSequence")

/* DISABLED: this deadlocks for some unknown reason
AsyncPrefixWhileSequenceTests.test("prefix while") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async
      .prefix { $0 < 3 }
      .collect()
    expectEqual(results, [1, 2])
  }
}
*/

var AsyncFlatMapSequenceTests = TestSuite("AsyncFlatMapSequence")

AsyncFlatMapSequenceTests.test("flat map") {
  runAsyncAndBlock {
    let results = await [1, 2, 3].async.flatMap { ($0..<4).async }.collect()
    let expected = [1, 2, 3].flatMap { $0..<4 }
    expectEqual(results, expected)
  }
}

runAllTests()
