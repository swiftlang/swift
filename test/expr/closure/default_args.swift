// RUN: %swift -parse %s -verify

func simple_default_args() {
  var f1 : (Int) -> Int = {(x : Int = 1) in x+1} // expected-error{{default argument is only permitted for a non-curried function parameter}}
  var f2 : () -> Int = {(x : Int = 1) in x+1} // expected-error{{default argument is only permitted for a non-curried function parameter}} expected-error{{tuple types '()' and '(Int)' have a different number of elements (0 vs. 1)}}
}

