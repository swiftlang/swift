// RUN: %swift %s -verify

func non_fully_covered_switch(x : Int) -> Int {
  switch x {
    case 0:
     x++
    case 3:
     x--
  } // expected-error{{switch must be exhaustive}}
  return x;
}

func non_fully_covered_switch_no_case_clauses(x : Int) -> Int {
  switch x {
  } // expected-error{{switch must be exhaustive}}
  return x;
}
