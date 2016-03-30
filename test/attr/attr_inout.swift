// RUN: %target-parse-verify-swift

func f(x : inout Int) { } // okay

func h(_ : inout Int) -> (inout Int) -> (inout Int) -> Int { }

func ff(x: (inout Int, inout Float)) { } //  expected-error {{'inout' is only valid in parameter lists}}  

enum inout_carrier {
  case carry(inout Int) // expected-error {{'inout' is only valid in parameter lists}}
}

func deprecated(inout x: Int) {} // expected-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}
