// RUN: %target-typecheck-verify-swift

struct IntRange<Int> : Sequence, IteratorProtocol {
  typealias Element = (Int, Int)
  func next() -> (Int, Int)? {}

  typealias Iterator = IntRange<Int>
  func makeIterator() -> IntRange<Int> { return self }
}

func for_each(r: Range<Int>, iir: IntRange<Int>) { // expected-note 2 {{did you mean 'r'?}}
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
  for i r { // expected-error 2{{expected ';' in 'for' statement}} expected-error {{use of unresolved identifier 'i'}} expected-error {{'Range<Int>' is not convertible to 'Bool'}}
  }
  for i in CountableRange(r) sum = sum + i; // expected-error{{expected '{' to start the body of for-each loop}}
}
