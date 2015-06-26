// RUN: %target-parse-verify-swift

func simple_default_args() {
  var f1 : (Int) -> Int = {(x : Int = 1) in x+1} // expected-error{{default argument is only permitted for a non-curried function parameter}}
  var f2 : () -> Int = {(x : Int = 1) in x+1} // expected-error{{'(Int) -> Int' is not convertible to '() -> Int'}} expected-error {{default argument is only permitted for a non-curried function parameter}}
}

func func_default_args() {
  func has_default_args(x x: Int = 1) -> Int { return x+1 }
  var _ : (Int) -> Int = has_default_args // okay
  var _ : () -> Int = has_default_args // expected-error{{(x: Int) -> Int' is not convertible to '() -> Int}}
}

