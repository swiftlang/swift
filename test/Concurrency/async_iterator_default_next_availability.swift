// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx15.0 -swift-version 6

// REQUIRES: OS=macosx, concurrency

struct Counter: AsyncIteratorProtocol {
  typealias Element = Int
  typealias Failure = Never

  mutating func next(isolation actor: isolated (any Actor)?) async -> Int? {
    0
  }
}

func testDefaultNext() async {
  var iterator = Counter()
  _ = await iterator.next() // expected-error {{'next()' is only available in SwiftStdlib 6.1 or newer}}
  // expected-note@-1 {{add @available attribute to enclosing global function}}
  // expected-note@-2 {{add 'if #available' version check}}
}

func testDefaultNextWithAvailabilityCheck() async {
  var iterator = Counter()
  if #available(SwiftStdlib 6.1, *) {
    _ = await iterator.next()
  }
}
