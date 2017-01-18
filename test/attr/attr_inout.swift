// RUN: %target-typecheck-verify-swift

func f(x : inout Int) { } // okay

func h(_ : inout Int) -> (inout Int) -> (inout Int) -> Int { }

func ff(x: (inout Int, inout Float)) { } //  expected-error {{'inout' may only be used on parameters}}

enum inout_carrier {
  case carry(inout Int) // expected-error {{'inout' may only be used on parameters}}
}

func deprecated(inout x: Int) {} // expected-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}
