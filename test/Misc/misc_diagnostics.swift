// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

var roomName : String? = nil

if let realRoomName = roomName as! NSString { // expected-warning{{always fails}} expected-error{{use 'as?'}}
			
}

var pi = 3.14159265358979
var d: CGFloat = 2.0
var dpi:CGFloat = d*pi // expected-error{{binary operator '*' cannot be applied to operands of type 'CGFloat' and 'Double'}}

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