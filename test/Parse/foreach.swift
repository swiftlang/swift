// RUN: %target-parse-verify-swift

struct IntRange<Int> : SequenceType, GeneratorType {
  typealias Element = (Int, Int)
  func next() -> (Int, Int)? {}

  typealias Generator = IntRange<Int>
  func generate() -> IntRange<Int> { return self }
}

func for_each(r: Range<Int>, iir: IntRange<Int>) {
  var sum = 0

  // Simple foreach loop, using the variable in the body
  for i in r {
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
  for i r { // expected-error 2{{expected ';' in 'for' statement}} expected-error {{use of unresolved identifier 'i'}}
  }
  for i in r sum = sum + i; // expected-error{{expected '{' to start the body of for-each loop}}
}
