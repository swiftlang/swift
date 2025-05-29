// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple
// REQUIRES: concurrency
import _Concurrency

struct AsyncRange<Bound: Comparable & Strideable>: AsyncSequence, AsyncIteratorProtocol where Bound.Stride : SignedInteger {
  var range: Range<Bound>.Iterator
  typealias Element = Bound
  mutating func next() async -> Element? { return range.next() }
  func cancel() { }

  func makeAsyncIterator() -> Self { return self }
}

struct AsyncIntRange<Int> : AsyncSequence, AsyncIteratorProtocol {
  typealias Element = (Int, Int)
  func next() async -> (Int, Int)? {}

  func cancel() { }

  typealias AsyncIterator = AsyncIntRange<Int>
  func makeAsyncIterator() -> AsyncIntRange<Int> { return self }
}

func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async { // expected-note {{'r' declared here}}
  var sum = 0

  // Simple foreach loop, using the variable in the body
  for await i in r {
    sum = sum + i
  }
  // Check scoping of variable introduced with foreach loop
  i = 0 // expected-error{{cannot find 'i' in scope; did you mean 'r'?}}

  // For-each loops with two variables and varying degrees of typedness
  for await (i, j) in iir {
    sum = sum + i + j
  }
  for await (i, j) in iir {
    sum = sum + i + j
  }
  for await (i, j) : (Int, Int) in iir {
    sum = sum + i + j
  }

  // Parse errors
  // FIXME: Bad diagnostics; should be just 'expected 'in' after for-each patter'.
  for await i r { // expected-error {{found an unexpected second identifier in constant declaration}}
  }         // expected-note @-1 {{join the identifiers together}}
            // expected-note @-2 {{join the identifiers together with camel-case}}
            // expected-error @-3 {{expected 'in' after for-each pattern}}
            // expected-error @-4 {{expected Sequence expression for for-each loop}}
  for await i in r sum = sum + i; // expected-error{{expected '{' to start the body of for-each loop}}
}
