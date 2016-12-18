// RUN: %target-typecheck-verify-swift

func simple_default_args() {
  // <rdar://problem/22753605> QoI: bad diagnostic when closure has default argument
  let _ : (Int) -> Int = {(x : Int = 1) in x+1} // expected-error{{default arguments are not allowed in closures}} {{36-39=}}
  let _ : () -> Int = {(_ x : Int = 1) in x+1} // expected-error{{contextual closure type '() -> Int' expects 0 arguments, but 1 was used in closure body}} expected-error {{default arguments are not allowed in closures}} {{35-38=}}
  let _ : () -> Int = {(_ x : Int) in x+1} // expected-error{{contextual closure type '() -> Int' expects 0 arguments, but 1 was used in closure body}}
}

func func_default_args() {
  func has_default_args(x: Int = 1) -> Int { return x+1 }
  var _ : (Int) -> Int = has_default_args // okay
  var _ : () -> Int = has_default_args // expected-error{{cannot convert value of type '(Int) -> Int' to specified type '() -> Int'}}
}

