// RUN: %target-parse-verify-swift

struct X { }

// Simple examples
struct X1 {
  var stored: Int

  subscript(i: Int) -> Int {
    get {
      return stored
    }
    mutating
    set {
      stored = newValue
    }
  }
}

struct X2 {
  var stored: Int

  subscript(i: Int) -> Int {
    get {
      return stored + i
    }
    set(v) {
      stored = v - i
    }
  }
}

struct X3 {
  var stored: Int

  subscript(_: Int) -> Int {
    get {
      return stored
    }
    set(v) {
      stored = v
    }
  }
}

struct X4 {
  var stored: Int

  subscript(i: Int, j: Int) -> Int {
    get {
      return stored + i + j
    }
    mutating
    set(v) {
      stored = v + i - j
    }
  }
}

struct Y1 {
  var stored: Int
  // FIXME: diagnostic spew is horrible here
  subscript(i, j: Int) -> Int { // expected-error 3{{use of undeclared type 'i'}}
    get {
      return stored + j
    }
    set {
      stored = j
    }
  }
}

// Parsing errors
// FIXME: Recovery here is horrible
struct A0 {
  subscript // expected-error{{expected '(' for subscript parameters}}
    i : Int // expected-error{{expected declaration}}
     -> Int { 
    get {
      return stored
    }
    set {
      stored = value
    }
  }
}

struct A1 {
  subscript (i : Int) // expected-error{{expected '->' for subscript element type}}
     Int {  // expected-error{{expected declaration}}
    get {
      return stored
    }
    set {
      stored = value
    }
  }
}

struct A2 {
  subscript (i : Int) -> // expected-error{{expected subscripting element type}}
     {  // expected-error{{expected declaration}}
    get {
      return stored
    }
    set {
      stored = value
    }
  }
}

struct A3 {
  subscript(i : Int) // expected-error {{expected '->' for subscript element type}}
  { // expected-error {{expected declaration}}
    get {
      return i
    }
  }
}

struct A4 {
  subscript(i : Int) { // expected-error {{expected '->' for subscript element type}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}}
    get {
      return i
    }
  }
}

struct A5 {
  subscript(i : Int) -> Int // expected-error {{expected '{' for subscripting}}
}

struct A6 {
  subscript(i: Int)(j: Int) -> Int { // expected-error {{expected '->' for subscript element type}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}}
    get {
      return i + j
    }
  }
}

struct A7 {
  static subscript(a: Int) -> Int { // expected-error {{subscript cannot be marked 'static'}}
    get {
      return 42
    }
  }
}

struct A7b {
  class subscript(a: Float) -> Int { // expected-error {{subscript cannot be marked 'class'}}
    get {
      return 42
    }
  }
}

struct A8 {
  subscript(i : Int) -> Int // expected-error{{expected '{' for subscripting}}
    get { // expected-error{{expected declaration}}
      return stored
    }
    set {
      stored = value
    }
  }
} // expected-error{{extraneous '}' at top level}}

