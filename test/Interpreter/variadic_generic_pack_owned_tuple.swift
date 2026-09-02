// RUN: %target-run-simple-swift

// REQUIRES: executable_test

// Test that passing pack expansion tuples to functions with @pack_owned
// parameters works correctly, especially when called through a generic wrapper.
// This is a regression test for a bug where SILGen failed to forward the
// cleanup of the tuple storage when transferring ownership to the pack,
// resulting in a double-free crash.

import StdlibUnittest

var packOwnedTupleTests = TestSuite("PackOwnedTuple")

// Protocol with associated type for the full pattern
protocol Mutator {
  associatedtype Value
  var seeds: [Value] { get }
  func mutate(_ input: Value) -> [Value]
}

struct SimpleMutator<T>: Mutator {
  typealias Value = T
  let seeds: [T]
  let mutateFn: (T) -> [T]

  init(seeds: [T], mutate: @escaping (T) -> [T] = { _ in [] }) {
    self.seeds = seeds
    self.mutateFn = mutate
  }

  func mutate(_ input: T) -> [T] { mutateFn(input) }
}

// Class that stores a closure capturing pack expansion tuple array
final class Box<each Input> {
  typealias Seeds = () -> [(repeat each Input)]
  let seedsFn: Seeds

  // Init takes @pack_owned parameter
  init<each M: Mutator>(
    mutators: (repeat each M)
  ) where (repeat (each M).Value) == (repeat each Input) {
    // Extract seeds eagerly from each mutator
    let capturedSeeds = Self.extractSeeds(mutators: mutators)
    self.seedsFn = { capturedSeeds }
  }

  private static func extractSeeds<each M: Mutator>(
    mutators: (repeat each M)
  ) -> [(repeat each Input)] where (repeat (each M).Value) == (repeat each Input) {
    var result: [(repeat each Input)] = []

    // Collect all seed arrays
    var allSeeds: [[Any]] = []
    (repeat allSeeds.append((each mutators).seeds.map { $0 as Any }))

    // For single mutator, just map seeds directly
    if let seeds = allSeeds.first, allSeeds.count == 1 {
      for seed in seeds {
        func cast<V>(_ t: V.Type) -> V { seed as! V }
        let tuple: (repeat each Input) = (repeat cast((each Input).self))
        result.append(tuple)
      }
    }
    return result
  }

  func get() -> [(repeat each Input)] {
    return seedsFn()
  }
}

// Public API that forwards parameter packs
func api<each Input, each M: Mutator>(
  using mutators: repeat each M
) -> [(repeat each Input)] where (repeat (each M).Value) == (repeat each Input) {
  // Capture the pack - this creates a tuple (repeat each M)
  let captured: (repeat each M) = (repeat each mutators)
  // Pass to Box.init which takes @pack_owned
  let box = Box<repeat each Input>(mutators: captured)
  return box.get()
}

// Generic wrapper function to test the wrapped call pattern
func wrapper<R>(_ op: () -> R) -> R {
  return op()
}

// Test 1: Direct call to api function
packOwnedTupleTests.test("directCall") {
  let seeds: [String] = api(using: SimpleMutator(seeds: ["a", "b", "c"]))
  expectEqual(seeds.count, 3)
  expectEqual(seeds[0], "a")
  expectEqual(seeds[1], "b")
  expectEqual(seeds[2], "c")
}

// Test 2: Call through generic wrapper - this is the pattern that crashed
packOwnedTupleTests.test("wrappedCall") {
  let seeds: [String] = wrapper {
    api(using: SimpleMutator(seeds: ["x", "y", "z"]))
  }
  expectEqual(seeds.count, 3)
  expectEqual(seeds[0], "x")
  expectEqual(seeds[1], "y")
  expectEqual(seeds[2], "z")
}

// Test 3: Multiple wrapped calls in sequence
packOwnedTupleTests.test("multipleWrappedCalls") {
  var allSeeds: [[String]] = []

  for i in 0..<3 {
    let seeds: [String] = wrapper {
      api(using: SimpleMutator(seeds: ["item\(i)"]))
    }
    allSeeds.append(seeds)
  }

  expectEqual(allSeeds.count, 3)
  expectEqual(allSeeds[0], ["item0"])
  expectEqual(allSeeds[1], ["item1"])
  expectEqual(allSeeds[2], ["item2"])
}

// Test 4: Nested wrappers
packOwnedTupleTests.test("nestedWrappers") {
  let seeds: [Int] = wrapper {
    wrapper {
      wrapper {
        api(using: SimpleMutator(seeds: [1, 2, 3, 4, 5]))
      }
    }
  }
  expectEqual(seeds, [1, 2, 3, 4, 5])
}

// Test 5: Class types to verify reference counting
packOwnedTupleTests.test("classTypeSeeds") {
  class Ref {
    let value: Int
    init(_ v: Int) { value = v }
  }

  // Create refs outside wrapper to verify they survive
  let refs = [Ref(10), Ref(20), Ref(30)]

  let seeds: [Ref] = wrapper {
    api(using: SimpleMutator(seeds: refs))
  }

  expectEqual(seeds.count, 3)
  expectEqual(seeds[0].value, 10)
  expectEqual(seeds[1].value, 20)
  expectEqual(seeds[2].value, 30)
}

// Test 6: Throwing wrapper
packOwnedTupleTests.test("throwingWrapper") {
  func throwingWrapper<R>(_ op: () throws -> R) rethrows -> R {
    return try op()
  }

  // Using rethrows, no actual throws happens here
  let seeds: [String] = throwingWrapper {
    api(using: SimpleMutator(seeds: ["success"]))
  }
  expectEqual(seeds, ["success"])
}

// Test 7: Async wrapper (if available)
#if swift(>=5.5)
@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
func asyncTest() async {
  func asyncWrapper<R>(_ op: () async -> R) async -> R {
    return await op()
  }

  let seeds: [String] = await asyncWrapper {
    api(using: SimpleMutator(seeds: ["async"]))
  }
  expectEqual(seeds, ["async"])
}
#endif

// Test 8: Escaping closure that captures the result
packOwnedTupleTests.test("escapingCapture") {
  var capturedSeeds: [String]? = nil

  func captureInEscaping(_ op: @escaping () -> [String]) {
    capturedSeeds = op()
  }

  captureInEscaping {
    api(using: SimpleMutator(seeds: ["captured"]))
  }

  expectEqual(capturedSeeds, ["captured"])
}

// Test 9: Verify cleanup happens correctly by checking for memory issues
// across many iterations
packOwnedTupleTests.test("stressTest") {
  for _ in 0..<1000 {
    let _: [String] = wrapper {
      api(using: SimpleMutator(seeds: ["stress", "test"]))
    }
  }
  // If we get here without crashing, the cleanup is working correctly
}

// Test 10: Empty seeds array
packOwnedTupleTests.test("emptySeeds") {
  let seeds: [String] = wrapper {
    api(using: SimpleMutator<String>(seeds: []))
  }
  expectEqual(seeds.count, 0)
}

runAllTests()
