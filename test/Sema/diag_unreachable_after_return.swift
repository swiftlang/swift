// RUN: %target-parse-verify-swift

// Warn when the indentation is the same.
func f_returns_void() {}
func unreachable_returns_void() {
  return 
  f_returns_void() // expected-warning {{expression following 'return' is treated as an argument of the 'return'}} // expected-note{{indent the expression to silence this warning}}
}

func f_returns_Int() {}
func unreachable_returns_Int() {
  return 
  f_returns_Int() // expected-warning {{expression following 'return' is treated as an argument of the 'return'}} // expected-note{{indent the expression to silence this warning}}
}

// Do not warn when the indentation is differnt.
func reachable_returns_void() {
  return 
    f_returns_void() // no-warning
}

func reachable_returns_Int() {
  return 
f_returns_Int() // no-warning
}

