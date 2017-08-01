// RUN: %target-typecheck-verify-swift

struct IntRange<Int> : Sequence, IteratorProtocol {
  typealias Element = (Int, Int)
  func next() -> (Int, Int)? {}

  typealias Iterator = IntRange<Int>
  func makeIterator() -> IntRange<Int> { return self }
}

func for_each(r: Range<Int>, iir: IntRange<Int>) { // expected-note {{did you mean 'r'?}}
  var sum = 0

  // Simple foreach loop, using the variable in the body
  for i in CountableRange(r) {
    sum = sum + i
  }
  // Check scoping of variable introduced with foreach loop
  i = 0 // expected-error{{use of unresolved identifier 'i'}}

  // For-each loops with two variables and varying degrees of typedness
  for (i, j) in iir {
    sum = sum + i + j
  }
  for (i, j) in iir {
    sum = sum + i + j
  }
  for (i, j) : (Int, Int) in iir {
    sum = sum + i + j
  }

  // Parse errors
  // FIXME: Bad diagnostics; should be just 'expected 'in' after for-each patter'.
  for i r { // expected-error {{found an unexpected second identifier in constant declaration}}
  }         // expected-note @-1 {{join the identifiers together}}
            // expected-note @-2 {{join the identifiers together with camel-case}}
            // expected-error @-3 {{expected 'in' after for-each pattern}}
            // expected-error @-4 {{expected Sequence expression for for-each loop}}
  for i in CountableRange(r) sum = sum + i; // expected-error{{expected '{' to start the body of for-each loop}}
}
