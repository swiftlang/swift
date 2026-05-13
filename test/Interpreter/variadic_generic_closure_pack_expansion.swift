// RUN: %target-run-simple-swift

// REQUIRES: executable_test

// Test that passing pack expansion tuples to closures works correctly at runtime.
// This is a regression test for a bug where SILGen incorrectly used pack_element_get
// to read from an uninitialized pack instead of pack_element_set to store addresses
// into the pack. This caused the saved frame pointer to be overwritten, resulting
// in a crash on function return.

import StdlibUnittest

var packExpansionClosureTests = TestSuite("PackExpansionClosure")

// Test 1: Basic pack expansion tuple passed to throwing closure
packExpansionClosureTests.test("basicPackExpansion") {
  struct Runner<each Input> {
    func run(_ input: (repeat each Input), test: ((repeat each Input)) throws -> Void) rethrows {
      try test(input)
    }
  }

  var callCount = 0
  let runner = Runner<Int, String>()
  runner.run((42, "hello")) { a, b in
    expectEqual(a, 42)
    expectEqual(b, "hello")
    callCount += 1
  }
  expectEqual(callCount, 1)
}

// Test 2: Iterator pattern - pack expansion tuple from array iteration
// This specifically tests the bug where iterating over an array of pack expansion
// tuples and passing each to a closure would crash.
packExpansionClosureTests.test("iteratorPattern") {
  struct FuzzEngine<each Input> {
    func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) rethrows {
      for input in inputs {
        try test(input)
      }
    }
  }

  var values: [Int] = []
  let engine = FuzzEngine<Int>()
  engine.run(inputs: [(1), (2), (3)]) { value in
    values.append(value)
  }
  expectEqual(values, [1, 2, 3])
}

// Test 3: Multiple pack parameters
packExpansionClosureTests.test("multiplePackParameters") {
  struct MultiEngine<each Input> {
    func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) rethrows {
      for input in inputs {
        try test(input)
      }
    }
  }

  var results: [(Int, String, Double)] = []
  let engine = MultiEngine<Int, String, Double>()
  engine.run(inputs: [(1, "a", 1.0), (2, "b", 2.0)]) { i, s, d in
    results.append((i, s, d))
  }
  expectEqual(results.count, 2)
  expectEqual(results[0].0, 1)
  expectEqual(results[0].1, "a")
  expectEqual(results[0].2, 1.0)
  expectEqual(results[1].0, 2)
  expectEqual(results[1].1, "b")
  expectEqual(results[1].2, 2.0)
}

// Test 4: Single element pack (vanishing tuple case)
// When instantiated with a single type, the tuple (repeat each T) becomes
// just T, but the pack handling must still work correctly.
packExpansionClosureTests.test("singleElementPack") {
  struct SingleEngine<each Input> {
    func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) rethrows {
      for input in inputs {
        try test(input)
      }
    }
  }

  var sum = 0
  let engine = SingleEngine<Int>()
  engine.run(inputs: [(10), (20), (30)]) { value in
    sum += value
  }
  expectEqual(sum, 60)
}

// Test 5: Nested function calls with pack expansion closures
// This tests that the stack frame is correctly preserved across multiple
// levels of pack expansion closure calls.
packExpansionClosureTests.test("nestedCalls") {
  struct Nested<each T> {
    func outer(_ value: (repeat each T), process: ((repeat each T)) throws -> Int) rethrows -> Int {
      return try inner(value, transform: process)
    }

    func inner(_ value: (repeat each T), transform: ((repeat each T)) throws -> Int) rethrows -> Int {
      return try transform(value)
    }
  }

  let nested = Nested<Int, Int>()
  let result = nested.outer((5, 10)) { a, b in
    return a + b
  }
  expectEqual(result, 15)
}

// Test 6: Throwing closure with pack expansion
packExpansionClosureTests.test("throwingClosure") {
  struct ThrowingEngine<each Input> {
    func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) throws {
      for input in inputs {
        try test(input)
      }
    }
  }

  enum TestError: Error { case expected }

  var processedCount = 0
  let engine = ThrowingEngine<Int>()

  do {
    try engine.run(inputs: [(1), (2), (3)]) { value in
      processedCount += 1
      if value == 2 {
        throw TestError.expected
      }
    }
    expectUnreachable("Should have thrown")
  } catch {
    expectEqual(processedCount, 2)
  }
}

// Test 7: Pack expansion with class types (to test reference counting)
packExpansionClosureTests.test("classTypes") {
  class Counter {
    var value: Int
    init(_ v: Int) { value = v }
  }

  struct ClassEngine<each Input> {
    func run(inputs: [(repeat each Input)], test: ((repeat each Input)) throws -> Void) rethrows {
      for input in inputs {
        try test(input)
      }
    }
  }

  var sum = 0
  let engine = ClassEngine<Counter, Counter>()
  let c1 = Counter(10)
  let c2 = Counter(20)
  engine.run(inputs: [(c1, c2)]) { a, b in
    sum = a.value + b.value
  }
  expectEqual(sum, 30)
}

runAllTests()
