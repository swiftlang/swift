// RUN: %target-run-simple-swift(-O -Xfrontend -enable-sil-opaque-values -Xfrontend -sil-verify-all)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

import StdlibUnittest

@inline(never)
func identity<T>(_ value: T) async -> T { value }

@inline(never)
func makePair<T>(_ first: T, _ second: T) -> (T, T) { (first, second) }

// Specialization creates tuple storage whose lifetime crosses async-let
// cleanup. SROA must preserve its non-nested lifetime when splitting it.
@inline(never)
func collect<T: Sendable>(_ firstValue: T, _ secondValue: T) async -> (T, T) {
  async let first = identity(firstValue)
  async let second = identity(secondValue)
  let results = await (first, second)
  return makePair(results.0, results.1)
}

let tests = TestSuite("SROANonNested")

tests.test("FieldsSurviveAsyncLetCleanup") {
  let result = await collect((2, 3), (5, 7))
  expectEqual(2, result.0.0)
  expectEqual(3, result.0.1)
  expectEqual(5, result.1.0)
  expectEqual(7, result.1.1)
}

await runAllTestsAsync()
