// RUN: %target-typecheck-verify-swift

struct X { } // expected-note {{did you mean}}

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
struct A0 {
  subscript // expected-error {{expected '(' for subscript parameters}}
    i : Int
     -> Int {
    get {
      return stored
    }
    set {
      stored = value
    }
  }
  
  subscript -> Int { // expected-error {{expected '(' for subscript parameters}} {{12-12=()}}
    return 1
  }
}

struct A1 {
  subscript (i : Int) // expected-error{{expected '->' for subscript element type}}
     Int {
    get {
      return stored // expected-error{{use of unresolved identifier}}
    }
    set {
      stored = newValue// expected-error{{use of unresolved identifier}}
    }
  }
}

struct A2 {
  subscript (i : Int) -> // expected-error{{expected subscripting element type}}
     {
    get {
      return stored
    }
    set {
      stored = newValue // expected-error{{use of unresolved identifier}}
    }
  }
}

struct A3 {
  subscript(i : Int) // expected-error {{expected '->' for subscript element type}}
                     // expected-error@-1 {{expected subscripting element type}}
  {
    get {
      return i
    }
  }
}

struct A4 {
  subscript(i : Int) { // expected-error {{expected '->' for subscript element type}}
                       // expected-error@-1 {{expected subscripting element type}}
    get {
      return i
    }
  }
}

struct A5 {
  subscript(i : Int) -> Int // expected-error {{expected '{' in subscript to specify getter and setter implementation}}
}

struct A6 {
  subscript(i: Int)(j: Int) -> Int { // expected-error {{expected '->' for subscript element type}}
                                     // expected-error@-1 {{function types cannot have argument labels}}
                                     // expected-note@-2 {{did you mean}}
    get {
      return i + j // expected-error {{use of unresolved identifier}}
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

struct A8 {
  subscript(i : Int) -> Int // expected-error{{expected '{' in subscript to specify getter and setter implementation}}
    get {
      return stored
    }
    set {
      stored = value
    }
  }

struct A9 {
  subscript x() -> Int { // expected-error {{subscripts cannot have a name}} {{13-14=}}
    return 0
  }
}

struct A10 {
  subscript x(i: Int) -> Int { // expected-error {{subscripts cannot have a name}} {{13-14=}}
    return 0
  }
  subscript x<T>(i: T) -> Int { // expected-error {{subscripts cannot have a name}} {{13-14=}}
    return 0
  }
}

struct A11 {
  subscript x y : Int -> Int { // expected-error {{expected '(' for subscript parameters}}
    return 0
  }
}

} // expected-error{{extraneous '}' at top level}} {{1-3=}}
