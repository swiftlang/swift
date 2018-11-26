// RUN: %target-typecheck-verify-swift

// The first function argument does not have a label; the others do.
func f1(_ a: Int, b: Int) { }
func f1(_ a: Int, d: Int) { } // okay: names differ

func f2(_ a: Int, b: Int) { } // expected-note{{'f2(_:b:)' previously declared here}}
func f2(_ z: Int, b: Int) { } // expected-error{{invalid redeclaration of 'f2(_:b:)'}}

class X {
  // Initializer arguments are API by default.
  init(x: Int) { }
  init(y: Int) { } // okay

  // Method arguments after the first are API by default
  func f1(_ a: Int, b: Int) { }
  func f1(_ a: Int, c: Int) { } // okay

  func f2(_ a: Int, b: Int) { } // expected-note{{'f2(_:b:)' previously declared here}}
  func f2(_ A: Int, b: Int) { } // expected-error{{invalid redeclaration of 'f2(_:b:)'}}
}

protocol P {
  func g1(_ value: Int)
  func g2(_ value: Int, other: Int)
  func g3(_ value: Int, other: Int, third: Int) // expected-note{{requirement 'g3(_:other:third:)' declared here}}
  static func g4(_ value: Int)
}

class PX : P {
  func g1(_ x: Int) { } // okay
  func g2(_ x: Int, other: Int) { } // okay
  func g3(_ x: Int, y: Int, third: Int) { } // expected-error{{method 'g3(_:y:third:)' has different argument labels from those required by protocol 'P' ('g3(_:other:third:)')}} {{21-21=other }}

  class func g4(_ x: Int) { }
}

// Default arguments have no effect on argument labels.
func f3(_ a: Int, b: Int = 5, c: Int = 6) { }
// expected-note@-1{{'f3(_:b:c:)' previously declared here}}
func f3(_ a: Int, b: Int, c: Int) { }
// expected-error@-1{{invalid redeclaration of 'f3(_:b:c:)'}}

class DefArg {
  func f(_ a: Int = 17) { } // okay: no label implied
  func f(a a: Int) { } // expected-warning{{extraneous duplicate parameter name; 'a' already has an argument label}} {{10-12=}}
}

struct Subscripts1 {
  subscript (i i: Int) -> Int {
    get { return i }
  }

  subscript (j j: Int) -> Int {
    get { return j }
  }
}

struct Subscripts2 {
  subscript (i: Int) -> Int { // expected-note{{'subscript(_:)' previously declared here}}
    get { return i }
  }

  subscript (j: Int) -> Int { // expected-error{{invalid redeclaration of 'subscript(_:)'}}
    get { return j }
  }
}


func f4(_ a: Int) -> (Int) -> () { return { b in () } }
func f5(_ a: Int) -> (_ b: Int) -> () { return { b in () } }

func testFunctions(_ i: Int, x: X) {
  f4(i)(i)
  f4(i)(b: i) // expected-error{{extraneous argument label 'b:' in call}} {{9-12=}}
  f5(i)(i)
  f5(i)(b: i) // expected-error{{extraneous argument label 'b:' in call}}{{9-12=}}
}

struct Y {
  func m0(_ a: Int) -> (Int) -> () { return { b in () } }
  func m1(_ a: Int) -> (_ b: Int) -> () { return { b in () } }

  func m2(_ a: Int) -> (Int, Int) -> () { return { b, c in () } }
  func m3(_ a: Int) -> (_ b: Int, _ c2: Int) -> () { return { b, c in () } }

  subscript (x: Int) -> Int {
    get { return x }
  }

  subscript (y y: String) -> String {
    get { return y }
  }
}

func testMethods(_ i: Int, x: Y) {
  x.m0(i)(i)
  x.m0(i)(b: i) // expected-error{{extraneous argument label 'b:' in call}} {{11-14=}}
  x.m1(i)(i)
  x.m1(i)(i) 
  x.m2(i)(i, c: i) // expected-error{{extraneous argument label 'c:' in call}} {{14-17=}}
  x.m2(i)(i, i)
  x.m3(i)(b: i, i) // expected-error{{extraneous argument label 'b:' in call}}{{11-14=}}
  x.m3(i)(b: i, c2: i) // expected-error{{extraneous argument labels 'b:c2:' in call}}{{11-14=}}{{17-21=}}
}

func testSubscripts(_ i: Int, s: String, x: Y) {
  var i2 = x[i]
  var i3 = x[x: i] // expected-error{{cannot subscript a value of type 'Y' with an index of type '(x: Int)'}}
  // expected-note @-1 {{overloads for 'subscript' exist with these partially matching parameter lists: (Int), (y: String)}}
  var s2 = x[y: s]
  var s3 = x[s]  // expected-error{{cannot convert value of type 'String' to expected argument type 'Int'}}
}

// Operators
func +(_ a: String,
       b b: Double) { } // expected-error{{operator cannot have keyword arguments}} {{8-10=}}

func +(a: Double, b: String) -> (Int) -> (_ d: Int) -> () {
  return { c in { e in () } }
}
