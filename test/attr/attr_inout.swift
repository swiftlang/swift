// RUN: %target-parse-verify-swift

func f(x : inout Int) { } // okay

func h(_ : inout Int) -> (inout Int) -> (inout Int) -> Int { }

func ff(x: (inout Int, inout Float)) { } //  expected-error {{'inout' is only valid in parameter lists}}  

enum inout_carrier {
  case carry(inout Int) // expected-error {{'inout' is only valid in parameter lists}}
}
