// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

// This test verifies that in an async context, overload resolution
// is correct for a type that conforms to both Sequence and AsyncSequence.
// Since generalizing Sequence methods for typed throws, the
// overload resolution prefers the corresponding AsyncSequence methods
// which cannot be generalized yet.
// To fix this, all rethrowing Sequence methods
// (except withContiguousStorageIfAvailable) were duplicated in a
// new extension (extension AsyncSequence where Self: Sequence).
@available(SwiftStdlib 5.1, *)
struct DualSequence: Sequence, AsyncSequence, IteratorProtocol, AsyncIteratorProtocol {
  func next() -> Int? { nil }

  func makeAsyncIterator() -> Self { self }
}

@available(SwiftStdlib 5.1, *)
func testSynchronousOverloadsArePicked() async {
  let ds = DualSequence()

  _ = ds.contains { _ in false }
  _ = ds.allSatisfy { _ in true }
  _ = ds.first { _ in false }
  _ = ds.count { _ in true }

  _ = ds.min { _, _ in true }
  _ = ds.max { _, _ in true }

  _ = ds.map { _ in 0 }
  _ = ds.filter { _ in false }
  _ = ds.flatMap { _ in [0] }
  _ = ds.compactMap { _ in 0 }

  _ = ds.reduce(0) { _, _ in 0 }
  _ = ds.reduce(into: 0) { _, _ in }

  _ = ds.starts(with: []) { _, _ in true }
  _ = ds.elementsEqual([]) { _, _ in true }
  _ = ds.lexicographicallyPrecedes([]) { _, _ in true }

  _ = ds.drop { _ in true }
  _ = ds.prefix { _ in false }
  _ = ds.split { _ in false }

  _ = ds.sorted { _, _ in true }

  ds.forEach { _ in }
}
