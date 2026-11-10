// RUN: %target-typecheck-verify-swift

func f(x : inout Int) { } // okay

func h(_ : inout Int) -> (inout Int) -> (inout Int) -> Int { }

func ff(x: (inout Int, inout Float)) { } //  expected-error 2{{'inout' may only be used on parameters}}

enum inout_carrier {
  case carry(inout Int) // expected-error {{'inout' may only be used on function or initializer parameters}} expected-astgen-error {{unexpected 'inout' keyword before parameter}} expected-astgen-error {{expected 'inout' in type specifier}}
}

func deprecated(inout x: Int) {} // expected-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}
