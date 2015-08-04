// RUN: %target-parse-verify-swift

func simple_default_args() {
  let _ : (Int) -> Int = {(x : Int = 1) in x+1} // expected-error{{default argument is only permitted for a non-curried function parameter}} {{36-39=}}
  let _ : () -> Int = {(x : Int = 1) in x+1} // expected-error{{cannot convert value of type '(Int) -> Int' to specified type '() -> Int'}} expected-error {{default argument is only permitted for a non-curried function parameter}} {{33-36=}}
  let _ : () -> Int = {(x : Int) in x+1} // expected-error{{cannot convert value of type '(Int) -> Int' to specified type '() -> Int'}}
}

func func_default_args() {
  func has_default_args(x x: Int = 1) -> Int { return x+1 }
  var _ : (Int) -> Int = has_default_args // okay
  var _ : () -> Int = has_default_args // expected-error{{cannot convert value of type '(x: Int) -> Int' to specified type '() -> Int'}}
}

