// RUN: %swift -I %S/../ %s -parse -verify

import swift

struct IntTupleRange {
  // FIXME: fill in details
}

func for_each(r : Range) {
  var sum = 0

  // Simple foreach loop, using the variable in the body
  foreach i in r {
    sum = sum + i
  }
  // Check scoping of variable introduced with foreach loop
  i = 0 // expected-error{{use of unresolved identifier 'i'}}

  // Foreach loops with two variables and varying degrees of typedness
  foreach (i, j) in r {
    sum = sum + i + j
  }
  foreach (i, j) in r {
    sum = sum + i + j
  }
  foreach (i, j) : (Int, Int) in r {
    sum = sum + i + j
  }

  // Parse errors
  foreach i r { // expected-error{{expected 'in' after foreach pattern}}
  }
  foreach i in r sum = sum + i; // expected-error{{expected '{' to start the body of foreach loop}}
}
