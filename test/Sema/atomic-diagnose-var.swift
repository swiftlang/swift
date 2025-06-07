// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: synchronization

import Synchronization

var a = Atomic(0) // expected-error {{variable of type 'Atomic<Int>' must be declared with a 'let'}}

let b = Atomic(0) // OK

class C {
  var d = Atomic(0) // expected-error {{variable of type 'Atomic<Int>' must be declared with a 'let'}}
  let e = Atomic(0) // OK
}

struct F: ~Copyable {
  var g = Atomic(0) // expected-error {{variable of type 'Atomic<Int>' must be declared with a 'let'}}
  let h = Atomic(0) // OK
}

func i(_: borrowing Atomic<Int>) {} // OK

func j(_: inout Atomic<Int>) {} // expected-error {{parameter of type 'Atomic<Int>' must be declared as either 'borrowing' or 'consuming'}}

func k(_: (inout Atomic<Int>) -> ()) {} // expected-error {{parameter of type 'Atomic<Int>' must be declared as either 'borrowing' or 'consuming'}}

func l(_: (borrowing Atomic<Int>) -> ()) {} // OK

func m() {
  let _: (Int, Int) -> Int = {
    var n = Atomic(0) // expected-error {{variable of type 'Atomic<Int>' must be declared with a 'let'}}
                 // expected-warning@-1 {{initialization of variable 'n' was never used; consider replacing with assignment to '_' or removing it}}

    return $0 + $1
  }
}

func o(_: consuming Atomic<Int>) {} // OK

func p(_: (consuming Atomic<Int>) -> ()) {} // OK
