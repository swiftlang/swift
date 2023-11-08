// RUN: %target-typecheck-verify-swift -enable-experimental-feature StaticExclusiveOnly

@_staticExclusiveOnly // expected-error{{@_staticExclusiveOnly can only be applied to noncopyable types}}
struct A {}

@_staticExclusiveOnly // OK
struct B: ~Copyable {
  mutating func change() { // expected-error {{@_staticExclusiveOnly type 'B' cannot have mutating function 'change()'}}
    print("123")
  }
}

let b0 = B() // OK

var b1 = B() // expected-error {{variable of type 'B' must be declared with a 'let'}}

class C {
  var b2 = B() // expected-error {{variable of type 'B' must be declared with a 'let'}}
  let b3 = B() // OK
}

struct D: ~Copyable {
  var b4 = B() // expected-error {{variable of type 'B' must be declared with a 'let'}}
  let b5 = B() // OK
}

func e(_: borrowing B) {} // OK

func f(_: inout B) {} // expected-error {{parameter of type 'B' must be declared as either 'borrowing' or 'consuming'}}

func g(_: (inout B) -> ()) {} // expected-error {{parameter of type 'B' must be declared as either 'borrowing' or 'consuming'}}

func h(_: (borrowing B) -> ()) {} // OK

func i() {
  let _: (Int, Int) -> Int = {
    var b6 = B() // expected-error {{variable of type 'B' must be declared with a 'let'}}
                 // expected-warning@-1 {{initialization of variable 'b6' was never used; consider replacing with assignment to '_' or removing it}}

    return $0 + $1
  }
}
