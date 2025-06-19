// RUN: %target-typecheck-verify-swift

// Marker protocol definition
@_marker protocol P1 {
  func f() // expected-error{{marker protocol 'P1' cannot have any requirements}}
}

@_marker protocol P2 {
  associatedtype AT // expected-error{{marker protocol 'P2' cannot have any requirements}}
}

@_marker protocol P3 {
  typealias A = Int
}

protocol P4 { } // expected-note{{'P4' declared here}}

@_marker protocol P5: P4 { } // expected-error{{marker protocol 'P5' cannot inherit non-marker protocol 'P4'}}

class C { }
@_marker protocol P5a: AnyObject { }  // okay
@_marker protocol P5b: C { }   // expected-error{{marker protocol 'P5b' cannot inherit class 'C'}}
@_marker protocol P5c where Self: C { }   // expected-error{{marker protocol 'P5c' cannot inherit class 'C'}}

// Legitimate uses of marker protocols.
extension P3 {
  func f() { }
}

extension Int: P3 { }
extension Array: P3 where Element: P3 { } // expected-note{{requirement from conditional conformance of '[Double]' to 'P3'}}

protocol P6: P3 { } // okay
@_marker protocol P7: P3 { } // okay

func genericOk<T: P3>(_: T) { }

func testGenericOk(i: Int, arr: [Int], nope: [Double], p3: P3, p3array: [P3]) {
  genericOk(i)
  genericOk(arr)
  genericOk(nope) // expected-error{{global function 'genericOk' requires that 'Double' conform to 'P3'}}
  genericOk(p3)
  genericOk(p3array)
}

// Incorrect uses of marker protocols in types.
func testNotOkay(a: Any) {
  var mp1: P3 = 17
  _ = mp1
  mp1 = 17

  if let mp2 = a as? P3 { _ = mp2 } // expected-error{{marker protocol 'P3' cannot be used in a conditional cast}}
  if let mp3 = a as? AnyObject & P3 { _ = mp3 } // expected-error{{marker protocol 'P3' cannot be used in a conditional cast}}
  if a is AnyObject & P3 { } // expected-error{{marker protocol 'P3' cannot be used in a conditional cast}}

  func inner(p3: P3) { }
}


@_marker protocol P8 { }
protocol P9: P8 { }

// Implied conditional conformance to P8 is okay because P8 is a marker
// protocol.
extension Array: P9 where Element: P9 { }

protocol P10 { }

extension Array: P10 where Element: P10, Element: P8 { }
// expected-error@-1{{conditional conformance to non-marker protocol 'P10' cannot depend on conformance of 'Element' to marker protocol 'P8'}}

@objc @_marker protocol P11 {}
// expected-error@-1 {{only classes (and their extensions), non-marker protocols, methods, initializers, properties, and subscript declarations can be declared '@objc'}}
