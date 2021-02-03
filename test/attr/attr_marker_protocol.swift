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

// Legitimate uses of marker protocols.
extension P3 {
  func f() { }
}

extension Int: P3 { }
extension Array: P3 where Element: P3 { } // expected-note{{requirement from conditional conformance of '[Double]' to 'P3'}}

protocol P6: P3 { } // okay
@_marker protocol P7: P3 { } // okay

func genericOk<T: P3>(_: T) { }

func testGenericOk(i: Int, arr: [Int], nope: [Double]) {
  genericOk(i)
  genericOk(arr)
  genericOk(nope) // expected-error{{global function 'genericOk' requires that 'Double' conform to 'P3'}}
}

// Incorrect uses of marker protocols in types.
func testNotOkay(a: Any) {
  var mp1: P3 = 17 // expected-error{{marker protocol 'P3' can only be used in generic constraints}}
  _ = mp1
  mp1 = 17

  if let mp2 = a as? P3 { _ = mp2 } // expected-error{{marker protocol 'P3' can only be used in generic constraints}}
  if let mp3 = a as? AnyObject & P3 { _ = mp3 } // expected-error{{marker protocol 'P3' can only be used in generic constraints}}
  if a is AnyObject & P3 { } // expected-error{{marker protocol 'P3' can only be used in generic constraints}}

  func inner(p3: P3) { } // expected-error{{marker protocol 'P3' can only be used in generic constraints}}
}

