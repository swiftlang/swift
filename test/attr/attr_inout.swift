// RUN: %target-parse-verify-swift

func f(inout x : Int) { } // okay

func h(inout _ : Int) -> (inout Int) -> (inout Int) -> Int { }

func ff(x: (inout Int, inout Float)) { } //  expected-error {{'inout' is only valid in parameter lists}}  expected-error {{'inout' is only valid in parameter lists}}

enum inout_carrier {
  case carry(inout Int) // expected-error {{'inout' is only valid in parameter lists}}
}
