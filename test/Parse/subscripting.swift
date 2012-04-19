// RUN: %swift -I %S/../ %s -verify

import swift

struct X { }

// Simple examples
struct X1 {
  stored : Int

  subscript (i : Int) -> Int {
    get { return stored}
    set { stored = value}
  }
}

struct X2 {
  stored : Int

  subscript (i : Int) -> Int {
    get { return stored + i }
    set (val) { stored = val - i }
  }
}

struct X3 {
  stored : Int

  subscript (_ : Int) -> Int {
    get { return stored  }
    set (val) { stored = val }
  }
}

struct X4 {
  stored : Int

  subscript (i : Int, j : Int) -> Int {
    get { return stored + i + j }
    set (val) { stored = val + i - j }
  }
}

struct Y1 { // expected-note{{to match this opening '{'}}
  stored : Int
  // FIXME: diagnostic spew is horrible here
  subscript (i, j : Int) -> Int { // expected-error{{function parameter must have an explicit type}} expected-error{{function parameter must have an explicit type}}  expected-error{{function parameter must have an explicit type}} 
    get { return stored + j }
    set { stored = j }
  }
}

// Parsing errors
// FIXME: Recovery here is horrible
struct A0 { // expected-note{{to match this opening '{'}}
  subscript 
    i : Int // expected-error{{expected '(' for subscript parameters}}
     -> Int { 
    get { return stored }
    set { stored = value }
  }
}

struct A1 { // expected-note{{to match this opening '{'}}
  subscript (i : Int)
     Int {  // expected-error{{expected '->' for subscript element type}}
    get { return stored }
    set { stored = value }
  }  
}

struct A2 { // expected-note{{to match this opening '{'}}
  subscript (i : Int) -> 
     {  // expected-error{{expected subscripting element type}}
    get { return stored }
    set { stored = value }
  }  
}

struct A3 { // expected-note{{to match this opening '{'}}
  subscript (i : Int) -> Int
    get { return stored } // expected-error{{expected '{' for subscripting}}
    set { stored = value }
  }  
} // expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}}