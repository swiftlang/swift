// RUN: %swift %s -verify

import swift

// Simple examples
struct X1 {
  stored : Int

  subscript (i : Int) -> Int {
    get { return stored }
    set { stored = value }
  }
}

struct X2 {
  stored : Int

  subscript (i : Int) -> Int {
    get { return stored }
    set (val) { stored = val }
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
} // expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}} expected-error{{expected '}' in struct}}