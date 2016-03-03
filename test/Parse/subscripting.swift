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
  subscript(_: i, j: Int) -> Int { // expected-error {{use of undeclared type 'i'}}
    get {
      return stored + j
    }
    set {
      stored = j
    }
  }
}

// Mutating getters on constants (https://bugs.swift.org/browse/SR-845)
struct Y2 {
  subscript(_: Int) -> Int {
    mutating get { return 0 }
  }
}

let y2 = Y2() // expected-note{{change 'let' to 'var' to make it mutable}}{{1-4=var}}
_ = y2[0] // expected-error{{cannot use mutating getter on immutable value: 'y2' is a 'let' constant}}

// Parsing errors
// FIXME: Recovery here is horrible
struct A0 { // expected-note{{in declaration of 'A0'}}
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

struct A1 { // expected-note{{in declaration of 'A1'}}
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

struct A2 { // expected-note{{in declaration of 'A2'}}
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

struct A3 { // expected-note{{in declaration of 'A3'}}
  subscript(i : Int) // expected-error {{expected '->' for subscript element type}}
  { // expected-error {{expected declaration}}
    get {
      return i
    }
  }
}

struct A4 { // expected-note{{in declaration of 'A4'}}
  subscript(i : Int) { // expected-error {{expected '->' for subscript element type}} expected-error {{consecutive declarations on a line must be separated by ';'}} {{21-21=;}} expected-error {{expected declaration}}
    get {
      return i
    }
  }
}

struct A5 {
  subscript(i : Int) -> Int // expected-error {{expected '{' in subscript to specify getter and setter implementation}}
}

struct A6 { // expected-note{{in declaration of 'A6'}}
  subscript(i: Int)(j: Int) -> Int { // expected-error {{expected '->' for subscript element type}} expected-error {{consecutive declarations on a line must be separated by ';'}} {{20-20=;}} expected-error {{expected declaration}}
    get {
      return i + j
    }
  }
}

struct A7 {
  static subscript(a: Int) -> Int { // expected-error {{subscript cannot be marked 'static'}} {{3-10=}}
    get {
      return 42
    }
  }
}

struct A7b {
  class subscript(a: Float) -> Int { // expected-error {{subscript cannot be marked 'class'}} {{3-9=}}
    get {
      return 42
    }
  }
}

struct A8 { // expected-note{{in declaration of 'A8'}}
  subscript(i : Int) -> Int // expected-error{{expected '{' in subscript to specify getter and setter implementation}}
    get { // expected-error{{expected declaration}}
      return stored
    }
    set {
      stored = value
    }
  }
} // expected-error{{extraneous '}' at top level}} {{1-3=}}
