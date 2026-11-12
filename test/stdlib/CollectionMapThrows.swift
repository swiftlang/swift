// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

struct TestError: Error, Equatable {
  var index: Int
}

let sizes = [1, 2, 3, 7, 8, 9, 15, 16, 17, 31, 32, 33, 64, 100]

func throwPositions(_ n: Int) -> [Int] {
  Array(Set([0, 1, n / 2, n - 1].filter { $0 >= 0 && $0 < n })).sorted()
}

var CollectionMapThrows = TestSuite("CollectionMapThrows")

CollectionMapThrows.test("NonTrivialElements/ContiguousSource") {
  for n in sizes {
    for k in throwPositions(n) {
      let source = Array(0..<n)
      do throws(TestError) {
        let _: [LifetimeTracked] = try source.map { (x: Int) throws(TestError) in
          if x == k { throw TestError(index: x) }
          return LifetimeTracked(x)
        }
        expectUnreachable("map should have thrown (n=\(n), k=\(k))")
      } catch {
        expectEqual(TestError(index: k), error)
      }
      expectEqual(0, LifetimeTracked.instances, "leak/over-release n=\(n) k=\(k)")
    }
  }
}

CollectionMapThrows.test("NonTrivialElements/NonContiguousSource") {
  for n in sizes {
    for k in throwPositions(n) {
      let source = MinimalCollection(elements: Array(0..<n))
      do throws(TestError) {
        let _: [LifetimeTracked] = try source.map { (x: Int) throws(TestError) in
          if x == k { throw TestError(index: x) }
          return LifetimeTracked(x)
        }
        expectUnreachable("map should have thrown (n=\(n), k=\(k))")
      } catch {
        expectEqual(TestError(index: k), error)
      }
      expectEqual(0, LifetimeTracked.instances, "leak/over-release n=\(n) k=\(k)")
    }
  }
}

CollectionMapThrows.test("NonTrivialElements/NoThrow") {
  for n in [0] + sizes {
    do {
      let result = Array(0..<n).map { LifetimeTracked($0) }
      withExtendedLifetime(result) {
        expectEqual(n, result.count)
        expectEqual(Array(0..<n), result.map { $0.value })
        expectEqual(n, LifetimeTracked.instances)
      }
    }
    expectEqual(0, LifetimeTracked.instances, "leak n=\(n)")
  }
}

CollectionMapThrows.test("TrivialElements/ThrowThenRetry") {
  for n in sizes {
    for k in throwPositions(n) {
      let source = Array(0..<n)
      do throws(TestError) {
        _ = try source.map { (x: Int) throws(TestError) -> Int in
          if x == k { throw TestError(index: x) }
          return x &+ 5
        }
        expectUnreachable()
      } catch {
        expectEqual(TestError(index: k), error)
      }
      let ok = source.map { $0 &+ 5 }
      expectEqual(Array(5..<(n + 5)), ok)
      expectGE(ok.capacity, n)
    }
  }
}

CollectionMapThrows.test("UntypedThrows") {
  struct Untyped: Error {}
  for n in sizes {
    let k = n / 2
    do {
      let _: [LifetimeTracked] = try Array(0..<n).map { (x: Int) throws -> LifetimeTracked in
        if x == k { throw Untyped() }
        return LifetimeTracked(x)
      }
      expectUnreachable()
    } catch {
      expectTrue(error is Untyped)
    }
    expectEqual(0, LifetimeTracked.instances, "leak n=\(n)")
  }
}

runAllTests()
