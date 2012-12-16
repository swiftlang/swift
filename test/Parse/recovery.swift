// RUN: %swift %s -verify

class Subscripts {
  subscript(Int) -> Int {
    invalid tokens that should be skipped ; , ! // expected-error{{expected 'get' or 'set' to define variable access}}
    {
      (this tests paren and brace matching)
    }
    more invalid tokens that should be skipped,
    even though there were braces there
  }

  subscript(Char) -> Char { ... we just forgot to close the brace // expected-error{{expected '}' at end of variable get/set clause}} expected-note{{to match this opening '{'}}
  
  func exists() -> Bool { return true }
}

func test(a : Subscripts) -> () {
  a.exists() // no-warning
}


func garbage() -> () {
  var a : Int
  ] this line is invalid, but we will stop at the keyword below... // expected-error{{expected expression}}
  return a + "a" // expected-error{{no candidates found for binary operator '+'}}
}

func moreGarbage() -> () {
  ) this line is invalid, but we will stop at the declaration... // expected-error{{expected expression}}
  func a() -> Int { return 4 }
  return a() + "a" // expected-error{{no candidates found for binary operator '+'}}
}


class Container<T> {
  func exists() -> Bool { return true }
}

func useContainer() -> () {
  var a : Container<not a type {skip this greater: >} >, b : Int // expected-error{{expected '>' to complete generic argument list}} expected-note{{to match this opening '<'}}
  b = 5 // no-warning
  a.exists() // expected-warnin
}


class [! (skip this r_square: ]) ] BadAttributes { // expected-error{{expected an attribute name}}
  func exists() -> Bool { return true }
}

func test(a : BadAttributes) -> () {
  a.exists() // no-warning
}
