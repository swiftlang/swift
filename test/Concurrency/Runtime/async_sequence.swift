// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime

// TODO: This crashes on linux for some strange reason
// REQUIRES: OS=macosx

import StdlibUnittest

// Utility functions for closure based operators to force them into throwing
// and async and throwing async contexts.

@available(SwiftStdlib 5.1, *)
func throwing<T>(_ value: T) throws -> T {
  return value
}

@available(SwiftStdlib 5.1, *)
func asynchronous<T>(_ value: T) async -> T {
  return value
}

@available(SwiftStdlib 5.1, *)
func asynchronousThrowing<T>(_ value: T) async throws -> T {
  return value
}

@available(SwiftStdlib 5.1, *)
struct Failure: Error, Equatable {
  var value = 1
}

@available(SwiftStdlib 5.1, *)
func failable<T, E: Error>(
  _ results: [Result<T, E>]
) -> AsyncThrowingMapSequence<AsyncLazySequence<[Result<T, E>]>, T> {
  return results.async.map { try $0.get() }
}


@available(SwiftStdlib 5.1, *)
extension Sequence {
  @inlinable
  public var async: AsyncLazySequence<Self> {
    get {
      return AsyncLazySequence(self)
    }
  }
}

@available(SwiftStdlib 5.1, *)
public struct AsyncLazySequence<S: Sequence>: AsyncSequence {
  public typealias Element = S.Element
  public typealias AsyncIterator = Iterator

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

  @usableFromInline
  init(_ sequence: S) {
    self.sequence = sequence
  }

  @inlinable
  public func makeAsyncIterator() -> Iterator {
    return Iterator(sequence.makeIterator())
  }
}

@available(SwiftStdlib 5.1, *)
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

@available(SwiftStdlib 5.1, *)
extension AsyncSequence where Element: Equatable {
  func `throw`(_ error: Error, on element: Element) -> AsyncThrowingMapSequence<Self, Element> {
    return map { (value: Element) throws -> Element in
      if value == element { throw error }
      return value
    }
  }
}

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.1, *) {

    var AsyncLazySequenceTests = TestSuite("AsyncLazySequence")

    AsyncLazySequenceTests.test("iteration past first nil") {
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

    AsyncLazySequenceTests.test("iterate twice with iterators before usage") {
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

    var AsyncSequenceTests = TestSuite("AsyncSequence")

    AsyncSequenceTests.test("reduce") {
      let result = await [1, 2, 3].async.reduce(0) { partial, element in
        return partial + element
      }
      expectEqual(result, 1 + 2 + 3)
    }

    AsyncSequenceTests.test("reduce throws in closure") {
      do {
        _ = try await [1, 2, 3].async.reduce(0) { partial, element in
          if element == 2 { throw Failure(value: 42) }
          return partial + element
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("reduce throws upstream") {
      do {
        let _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .reduce(0) { partial, element in
            return partial + element
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("reduce into") {
      let result = await [1, 2, 3].async.reduce(into: 0) { partial, element in
        partial += element
      }
      expectEqual(result, 1 + 2 + 3)
    }

    AsyncSequenceTests.test("reduce into throws in closure") {
      do {
        _ = try await [1, 2, 3].async.reduce(into: 0) { partial, element in
          if element == 2 { throw Failure(value: 42) }
          partial += element
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("reduce into throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .reduce(into: 0) { partial, element in
            partial += element
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("contains predicate true") {
      let result = await [1, 2, 3].async.contains { $0 == 2 }
      expectTrue(result)
    }

    AsyncSequenceTests.test("contains predicate false") {
      let result = await [1, 2, 3].async.contains { $0 == 4 }
      expectFalse(result)
    }

    AsyncSequenceTests.test("contains predicate empty") {
      let result = await [Int]().async.contains { $0 == 4 }
      expectFalse(result)
    }

    AsyncSequenceTests.test("contains throws in closure") {
      do {
        _ = try await [1, 2, 3].async
          .contains {
            if $0 == 2 { throw Failure(value: 42) }
            return $0 == 3
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("contains throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .contains {
            return $0 == 3
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("all satisfy true") {
      let result = await [1, 2, 3].async.allSatisfy { $0 < 10 }
      expectTrue(result)
    }

    AsyncSequenceTests.test("all satisfy throws in closure") {
      do {
        _ = try await [1, 2, 3].async
          .allSatisfy {
            if $0 == 2 { throw Failure(value: 42) }
            return $0 < 10
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("all satisfy throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .allSatisfy {
            return $0 < 10
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("all satisfy false") {
      let result = await [1, 2, 3].async.allSatisfy { $0 > 10 }
      expectFalse(result)
    }

    AsyncSequenceTests.test("all satisfy empty") {
      let result = await [Int]().async.allSatisfy { $0 > 10 }
      expectTrue(result)
    }

    AsyncSequenceTests.test("contains true") {
      let result = await [1, 2, 3].async.contains(2)
      expectTrue(result)
    }

    AsyncSequenceTests.test("contains false") {
      let result = await [1, 2, 3].async.contains(4)
      expectFalse(result)
    }

    AsyncSequenceTests.test("contains throws upstream") {
      do {
        _ = try await [1, 2, 3].async.throw(Failure(value: 42), on: 2).contains(4)
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("contains empty") {
      let result = await [Int]().async.contains(4)
      expectFalse(result)
    }

    AsyncSequenceTests.test("first found") {
      let result = await [1, 2, 3].async.first { $0 > 1 }
      expectEqual(result, 2)
    }

    AsyncSequenceTests.test("first throws in closure") {
      do {
        _ = try await [1, 2, 3].async
          .first {
            if $0 == 2 { throw Failure(value: 42) }
            return $0 > 2
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("first throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .first {
            return $0 > 2
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("first empty") {
      let result = await [Int]().async.first { $0 > 1 }
      expectEqual(result, nil)
    }

    AsyncSequenceTests.test("min by found") {
      let result = await [1, 2, 3].async.min { $0 < $1 }
      expectEqual(result, 1)
    }

    AsyncSequenceTests.test("min by throws in closure") {
      do {
        _ = try await [1, 2, 3].async
          .min {
            if $0 == 2 { throw Failure(value: 42) }
            return $0 < $1
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("min by throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .min {
            return $0 < $1
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("min by empty") {
      let result = await [Int]().async.min { $0 < $1 }
      expectEqual(result, nil)
    }

    AsyncSequenceTests.test("max by found") {
      let result = await [1, 2, 3].async.max { $0 < $1 }
      expectEqual(result, 3)
    }

    AsyncSequenceTests.test("max by throws in closure") {
      do {
        _ = try await [1, 2, 3].async
          .max {
            if $0 == 2 { throw Failure(value: 42) }
            return $0 < $1
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("max by throws upstream") {
      do {
        _ = try await [1, 2, 3].async
          .throw(Failure(value: 42), on: 2)
          .max {
            return $0 < $1
          }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("max by empty") {
      let result = await [Int]().async.max { $0 < $1 }
      expectEqual(result, nil)
    }

    AsyncSequenceTests.test("min found") {
      let result = await [1, 2, 3].async.min()
      expectEqual(result, 1)
    }

    AsyncSequenceTests.test("min empty") {
      let result = await [Int]().async.min()
      expectEqual(result, nil)
    }

    AsyncSequenceTests.test("max found") {
      let result = await [1, 2, 3].async.max()
      expectEqual(result, 3)
    }

    AsyncSequenceTests.test("max empty") {
      let result = await [Int]().async.max()
      expectEqual(result, nil)
    }

    AsyncSequenceTests.test("collect") {
      let result = await [1, 2, 3].async.collect()
      expectEqual(result, [1, 2, 3])
    }

    AsyncSequenceTests.test("collect throws upstream") {
      do {
        _ = try await [1, 2, 3].async.throw(Failure(value: 42), on: 2).collect()
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }
    }

    AsyncSequenceTests.test("collect empty") {
      let result = await [Int]().async.collect()
      expectEqual(result, [])
    }

    var AsyncCompactMapTests = TestSuite("AsyncCompactMap")

    AsyncCompactMapTests.test("non nil values") {
      let result = await [1, 2, 3].async.compactMap { $0 }.collect()
      expectEqual(result, [1, 2, 3])
    }

    AsyncCompactMapTests.test("nil values") {
      let result = await [1, nil, 2, nil, 3, nil].async
        .compactMap { $0 }.collect()
      expectEqual(result, [1, 2, 3])
    }

    AsyncCompactMapTests.test("non nil values async") {
      let result = await [1, 2, 3].async
        .compactMap { await asynchronous($0) }.collect()
      expectEqual(result, [1, 2, 3])
    }

    AsyncCompactMapTests.test("nil values async") {
      let result = await [1, nil, 2, nil, 3, nil].async
        .compactMap { await asynchronous($0) }.collect()
      expectEqual(result, [1, 2, 3])
    }

    AsyncCompactMapTests.test("non nil values with throw") {
      do {
        let result = try await [1, 2, 3].async
          .compactMap { try throwing($0) }.collect()
        expectEqual(result, [1, 2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncCompactMapTests.test("nil values with throw") {
      do {
        let result = try await [1, nil, 2, nil, 3, nil].async
          .compactMap { try throwing($0) }.collect()
        expectEqual(result, [1, 2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncCompactMapTests.test("non nil values with async throw") {
      do {
        let result = try await [1, 2, 3].async
          .compactMap { try await asynchronousThrowing($0) }.collect()
        expectEqual(result, [1, 2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncCompactMapTests.test("nil values with async throw") {
      do {
        let result = try await [1, nil, 2, nil, 3, nil].async
          .compactMap { try await asynchronousThrowing($0) }.collect()
        expectEqual(result, [1, 2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncCompactMapTests.test("throwing in closure") {
      let seq = [1, nil, 2, nil, 3, nil].async
        .compactMap { value throws -> Int? in
          if value == 2 {
            throw Failure(value: 42)
          }
          return value
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncCompactMapTests.test("throwing upstream") {
      let seq = [1, nil, 2, nil, 3, nil].async
        .throw(Failure(value: 42), on: 2)
        .compactMap { value throws -> Int? in
          return value
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    var AsyncMapSequenceTests = TestSuite("AsyncMapSequence")

    AsyncMapSequenceTests.test("map values") {
      let results = await [1, 2, 3].async.map { "\($0)" }.collect()
      expectEqual(results, ["1", "2", "3"])
    }

    AsyncMapSequenceTests.test("map empty") {
      let results = await [Int]().async.map { "\($0)" }.collect()
      expectEqual(results, [])
    }

    AsyncMapSequenceTests.test("map throwing in closure") {
      let seq = [1, 2, 3].async
        .map { value throws -> String in
          if value == 2 {
            throw Failure(value: 42)
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
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, ["1"])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncMapSequenceTests.test("map throwing upstream") {
      let seq = [1, 2, 3].async
        .throw(Failure(value: 42), on: 2)
        .map { value throws -> String in
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
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, ["1"])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    var AsyncFilterSequenceTests = TestSuite("AsyncFilterSequence")

    AsyncFilterSequenceTests.test("filter out one item") {
      let results = await [1, 2, 3].async.filter { $0 % 2 != 0 } .collect()
      expectEqual(results, [1, 3])
    }

    AsyncFilterSequenceTests.test("filter out one item throwing") {
      do {
        let results = try await [1, 2, 3].async
          .filter { try throwing($0 % 2 != 0) }
          .collect()
        expectEqual(results, [1, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncFilterSequenceTests.test("filter out one item throwing async") {
      do {
        let results = try await [1, 2, 3].async
          .filter { try await asynchronousThrowing($0 % 2 != 0) }
          .collect()
        expectEqual(results, [1, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncFilterSequenceTests.test("filter out two items") {
      let results = await [1, 2, 3].async.filter { $0 % 2 == 0 } .collect()
      expectEqual(results, [2])
    }

    AsyncFilterSequenceTests.test("filter out all items") {
      let results = await [1, 2, 3].async.filter { _ in return false } .collect()
      expectEqual(results, [Int]())
    }

    AsyncFilterSequenceTests.test("filter out no items") {
      let results = await [1, 2, 3].async.filter { _ in return true } .collect()
      expectEqual(results, [1, 2, 3])
    }

    AsyncFilterSequenceTests.test("filter throwing in closure") {
      let seq = [1, 2, 3].async
        .filter {
          if $0 == 2 { throw Failure(value: 42) }
          return $0 % 2 != 0
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncFilterSequenceTests.test("filter throwing upstream") {
      let seq = [1, 2, 3].async
        .throw(Failure(value: 42), on: 2)
        .filter {
          return $0 % 2 != 0
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    var AsyncDropWhileSequenceTests = TestSuite("AsyncDropWhileSequence")

    AsyncDropWhileSequenceTests.test("drop one") {
      let results = await [1, 2, 3].async.drop { value -> Bool in
        return value == 1
      }.collect()
      expectEqual(results, [2, 3])
    }

    AsyncDropWhileSequenceTests.test("drop throwing in closure") {
      let seq = [1, 2, 3].async
        .drop {
          if $0 == 2 { throw Failure(value: 42) }
          return $0 == 1
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncDropWhileSequenceTests.test("drop throwing upstream") {
      let seq = [1, 2, 3].async
        .throw(Failure(value: 42), on: 2)
        .drop {
          return $0 == 1
        }
      var it = seq.makeAsyncIterator()
      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncDropWhileSequenceTests.test("drop one async") {
      let results = await [1, 2, 3].async.drop { value async -> Bool in
        return await asynchronous(value == 1)
      }.collect()
      expectEqual(results, [2, 3])
    }

    AsyncDropWhileSequenceTests.test("drop one throws") {
      do {
        let results = try await [1, 2, 3].async.drop { value throws -> Bool in
          return try throwing(value == 1)
        }.collect()
        expectEqual(results, [2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncDropWhileSequenceTests.test("drop one throws async") {
      do {
        let results = try await [1, 2, 3].async.drop { value async throws -> Bool in
          return try await asynchronousThrowing(value == 1)
        }.collect()
        expectEqual(results, [2, 3])
      } catch {
        expectUnreachable()
      }
    }

    AsyncDropWhileSequenceTests.test("drop none") {
      let results = await [1, 2, 3].async.drop { _ in return false }.collect()
      expectEqual(results, [1, 2, 3])
    }

    AsyncDropWhileSequenceTests.test("drop all") {
      let results = await [1, 2, 3].async.drop { _ in return true }.collect()
      expectEqual(results, [])
    }

    AsyncDropWhileSequenceTests.test("drop stops") {
      let results = await [1, 2, 3].async.drop { value in
        return value == 1 || value == 3
      }.collect()
      expectEqual(results, [2, 3])
    }

    var AsyncDropFirstSequenceTests = TestSuite("AsyncDropFirstSequence")

    AsyncDropFirstSequenceTests.test("drop 1") {
      let results = await [1, 2, 3].async.dropFirst().collect()
      expectEqual(results, [2, 3])
    }

    AsyncDropFirstSequenceTests.test("drop 2") {
      let results = await [1, 2, 3].async.dropFirst(2).collect()
      expectEqual(results, [3])
    }

    AsyncDropFirstSequenceTests.test("drop 0") {
      let results = await [1, 2, 3].async.dropFirst(0).collect()
      expectEqual(results, [1, 2, 3])
    }

    var AsyncPrefixSequenceTests = TestSuite("AsyncPrefixSequence")

    AsyncPrefixSequenceTests.test("prefix satisfied") {
      let results = await [1, 2, 3].async.prefix(2).collect()
      expectEqual(results, [1, 2])
    }

    AsyncPrefixSequenceTests.test("prefix partial satisfied") {
      let results = await [1, 2, 3].async.prefix(4).collect()
      expectEqual(results, [1, 2, 3])
    }

    AsyncPrefixSequenceTests.test("prefix 0") {
      let results = await [1, 2, 3].async.prefix(0).collect()
      expectEqual(results, [])
    }

    var AsyncPrefixWhileSequenceTests = TestSuite("AsyncPrefixWhileSequence")

    /* DISABLED: this deadlocks for some unknown reason
    AsyncPrefixWhileSequenceTests.test("prefix while") {
      let results = await [1, 2, 3].async
        .prefix { $0 < 3 }
        .collect()
      expectEqual(results, [1, 2])
    }
    */

    var AsyncFlatMapSequenceTests = TestSuite("AsyncFlatMapSequence")

    AsyncFlatMapSequenceTests.test("flat map") {
      let results = await [1, 2, 3].async.flatMap { ($0..<4).async }.collect()
      let expected = [1, 2, 3].flatMap { $0..<4 }
      expectEqual(results, expected)
    }

    AsyncFlatMapSequenceTests.test("flat map throwing in closure") {
      let seq = [1, 2, 3].async
        .flatMap { (value: Int) throws -> AsyncLazySequence<Range<Int>> in
          if value == 2 { throw Failure(value: 42) }
          return (value..<4).async
        }
      var it = seq.makeAsyncIterator()

      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1, 2, 3])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncFlatMapSequenceTests.test("flat map throwing upstream") {
      let seq = [1, 2, 3].async
        .throw(Failure(value: 42), on: 2)
        .flatMap {
          return ($0..<4).async
        }
      var it = seq.makeAsyncIterator()

      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1, 2, 3])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncFlatMapSequenceTests.test("flat map throwing inner") {
      @Sendable func buildSubSequence(
        _ start: Int
      ) -> AsyncThrowingMapSequence<AsyncLazySequence<Range<Int>>, Int> {
        return (start..<4).async.map { (value: Int) throws -> Int in
          if value == 2 { throw Failure(value: 42) }
          return value
        }
      }
      let seq = [1, 2, 3].async
        .flatMap(buildSubSequence)
      var it = seq.makeAsyncIterator()

      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    AsyncFlatMapSequenceTests.test("flat map throwing inner on throwing outer") {
      @Sendable func buildSubSequence(
        _ start: Int
      ) throws -> AsyncThrowingMapSequence<AsyncLazySequence<Range<Int>>, Int> {
        return (start..<4).async.map { (value: Int) throws -> Int in
          if value == 2 { throw Failure(value: 42) }
          return value
        }
      }
      let seq = [1, 2, 3].async
        .flatMap(buildSubSequence)
      var it = seq.makeAsyncIterator()

      var results = [Int]()

      do {
        while let value = try await it.next() {
          results.append(value)
        }
        expectUnreachable()
      } catch {
        expectEqual(error as? Failure, Failure(value: 42))
      }

      expectEqual(results, [1])

      do {
        let result = try await it.next()
        expectNil(result)
      } catch {
        expectUnreachable()
      }
    }

    }
    await runAllTestsAsync()
  }
}
