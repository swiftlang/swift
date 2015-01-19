// RUN: %target-parse-verify-swift

// Function arguments are not API by default, so this is okay.
func f1(a: Int, b: Int) { }
func f1(a c: Int, b d: Int) { } // okay: names differ

func f2(a: Int, b: Int) { } // expected-note{{'f2' previously declared here}}
func f2(c: Int, d: Int) { } // expected-error{{invalid redeclaration of 'f2'}}

class X {
  // Initializer arguments are API by default.
  init(x: Int) { }
  init(y: Int) { } // okay

  // Method arguments after the first are API by default
  func f1(a: Int, b: Int) { }
  func f1(a: Int, c: Int) { } // okay

  func f2(a: Int, b: Int) { } // expected-note{{'f2(_:b:)' previously declared here}}
  func f2(A: Int, b: Int) { } // expected-error{{invalid redeclaration of 'f2(_:b:)'}}
}

protocol P {
  func g1(value: Int)
  func g2(value: Int, other: Int)
  func g3(value: Int, other: Int, third: Int) // expected-note{{requirement 'g3(_:other:third:)' declared here}}
  static func g4(value: Int)
}

class PX : P {
  func g1(x: Int) { } // okay
  func g2(x: Int, other: Int) { } // okay
  func g3(x: Int, y: Int, third: Int) { } // expected-error{{method 'g3(_:y:third:)' has different argument names from those required by protocol 'P' ('g3(_:other:third:)')}}

  class func g4(x: Int) { }
}

// Default arguments imply keyword arguments
func f3(a: Int, b: Int = 5, #c: Int = 6) { } // expected-warning{{extraneous '#' in parameter; default argument implies keyword argument}}{{29-30=}}
// expected-note@-1{{'f3(_:b:c:)' previously declared here}}
func f3(a: Int, #b: Int, #c: Int) { } 
// expected-error@-1{{invalid redeclaration of 'f3(_:b:c:)'}}

class DefArg {
  func f(a: Int = 17) { } // expected-note{{'f(a:)' previously declared here}}
  func f(#a: Int) { } // expected-error{{nvalid redeclaration of 'f(a:)'}}
}

struct Subscripts1 {
  subscript (#i: Int) -> Int {
    get { return i }
  }

  subscript (#j: Int) -> Int {
    get { return j }
  }
}

struct Subscripts2 {
  subscript (i: Int) -> Int { // expected-note{{'subscript' previously declared here}}
    get { return i }
  }

  subscript (j: Int) -> Int { // expected-error{{invalid redeclaration of 'subscript'}}
    get { return j }
  }
}


func f4(a: Int)(_ b: Int) { }
func f5(a: Int)(b: Int) { }

func testFunctions(i: Int, x: X) {
  f4(i)(i)
  f4(i)(b: i) // expected-error{{extraneous argument label 'b:' in call}}
  f5(i)(i) // expected-error{{missing argument label 'b:' in call}}
  f5(i)(b: i)
}

struct Y {
  func m0(a: Int)(_ b: Int) { }
  func m1(a: Int)(b: Int) { }

  func m2(a: Int)(_ b: Int, _ c: Int) { }
  func m3(a: Int)(b: Int, c2 c: Int) { }

  subscript (x: Int) -> Int {
    get { return x }
  }

  subscript (#y: String) -> String {
    get { return y }
  }
}

func testMethods(i: Int, x: Y) {
  x.m0(i)(i)
  x.m0(i)(b: i) // expected-error{{extraneous argument label 'b:' in call}}
  x.m1(i)(i) // expected-error{{missing argument label 'b:' in call}}
  x.m1(i)(b: i) 
  x.m2(i)(i, c: i) // expected-error{{extraneous argument label 'c:' in call}}
  x.m2(i)(i, i)
  x.m3(i)(b: i, i) // expected-error{{missing argument label 'c2:' in call}}
  x.m3(i)(b: i, c2: i)
}

func testSubscripts(i: Int, s: String, x: Y) {
  var i2 = x[i]
  var i3 = x[x: i] // expected-error{{extraneous argument label 'x:' in subscript}}
  var s2 = x[y: s]
  var s3 = x[s]  // expected-error{{missing argument label 'y:' in subscript}}
}
