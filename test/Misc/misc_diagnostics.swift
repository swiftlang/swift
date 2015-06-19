// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

var roomName : String? = nil

if let realRoomName = roomName as! NSString { // expected-error {{initializer for conditional binding must have Optional type, not 'NSString'}} expected-warning {{cast from 'String?' to unrelated type 'NSString' always fails}}
			
}

var pi = 3.14159265358979
var d: CGFloat = 2.0
var dpi:CGFloat = d*pi // expected-error{{binary operator '*' cannot be applied to operands of type 'CGFloat' and 'Double'}} expected-note{{overloads for '*' exist with these partially matching parameter lists: (Double, Double), (CGFloat, CGFloat)}}

let ff: CGFloat = floorf(20.0) // expected-error{{'Float' is not convertible to 'CGFloat'}}

let total = 15.0
let count = 7
let median = total / count // expected-error {{binary operator '/' cannot be applied to operands of type 'Double' and 'Int'}} expected-note {{overloads for '/' exist with these partially matching parameter lists: (Int, Int), (Double, Double)}}

if (1) {} // expected-error{{'Int' is not convertible to 'BooleanType'}}
var a: [String] = [1] // expected-error{{'[Int]' is not convertible to '[String]'}}
var b: Int = [1, 2, 3] // expected-error{{'[Int]' is not convertible to 'Int'}}

var f1: Float = 2.0
var f2: Float = 3.0

var dd: Double = f1 - f2 // expected-error{{'Float' is not convertible to 'Double'}}

func f() -> Bool {
  return 1 + 1 // expected-error{{'Int' is not convertible to 'Bool'}}
}

// Test that nested diagnostics are properly surfaced.
func takesInt(i: Int) {}
func noParams() -> Int { return 0 }
func takesAndReturnsInt(i: Int) -> Int { return 0 }

takesInt(noParams(1)) // expected-error{{cannot invoke 'noParams' with an argument list of type '(Int)'}}
takesInt(takesAndReturnsInt("")) // expected-error{{cannot invoke 'takesAndReturnsInt' with an argument list of type '(String)'}} expected-note{{expected an argument list of type '(Int)'}}

// Test error recovery for type expressions.
class A {
    var a: Array<Int>
    init() {
        a = Array<Int // expected-error{{argument for generic parameter 'T' could not be inferred}} 
        // expected-error@-1 5{{expected member name or constructor call after type name}}
        // expected-note@-2 5{{add arguments after the type to construct a value of the type}}
        // expected-note@-3 5{{use '.self' to reference the type object}}
    }
}

func retV() { return true } // expected-error {{unexpected non-void return value in void function}}

func retAI() -> Int {
    let a = [""]
    let b = [""]
    return (a + b) // expected-error {{cannot convert return expression of type '[String]' to expected return type 'Int'}}
}

func testIS1() -> Int { return 0 }
let _: String = testIS1() // expected-error {{'Int' is not convertible to 'String'}}

func insertA<T>(inout array : [T], elt : T) {
  array.append(T); // expected-error {{cannot invoke 'append' with an argument list of type '((T).Type)'}}
}

// <rdar://problem/17875634> can't append to array of tuples
func test17875634() {
  var match: [(Int, Int)] = []
  var row = 1
  var col = 2
  var coord = (row, col)

  match += (1, 2) // expected-error{{binary operator '+=' cannot be applied to operands of type '[(Int, Int)]' and '(Int, Int)'}}
  match += (row, col) // expected-error{{binary operator '+=' cannot be applied to operands of type '[(Int, Int)]' and '(Int, Int)'}}
  match += coord // expected-error{{binary operator '+=' cannot be applied to operands of type '[(Int, Int)]' and '(Int, Int)'}}

  match.append(row, col) // expected-error{{extra argument in call}}
  match.append(1, 2) // expected-error{{extra argument in call}}

  match.append(coord)
  match.append((1, 2))

  // Make sure the behavior matches the non-generic case.
  struct FakeNonGenericArray {
    func append(p: (Int, Int)) {}
  }
  let a2 = FakeNonGenericArray()
  a2.append(row, col) // expected-error{{extra argument in call}}
  a2.append(1, 2) // expected-error{{extra argument in call}}
  a2.append(coord)
  a2.append((1, 2))
}

// <rdar://problem/20770032> Pattern matching ranges against tuples crashes the compiler
func test20770032() {
  if case let 1...10 = (1, 1) { // expected-warning{{'let' pattern has no effect; sub-pattern didn't bind any variables}} expected-error{{binary operator '~=' cannot be applied to operands of type 'Range<Int>' and '(Int, Int)'}}
  }
}
