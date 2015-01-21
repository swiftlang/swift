// RUN: %target-parse-verify-swift

import Foundation
import CoreGraphics

var roomName : String? = nil

if let realRoomName = roomName as! NSString { // expected-warning{{always fails}} expected-error{{use 'as?'}}
			
}

var pi = 3.14159265358979
var d: CGFloat = 2.0
var dpi:CGFloat = d*pi // expected-error{{binary operator '*' cannot be applied to operands of type 'CGFloat' and 'Double'}}

let ff: CGFloat = floorf(20.0) // expected-error{{cannot invoke 'floorf' with an argument list of type '(Double)'}} expected-note{{expected an argument list of type '(Float)'}}

let total = 15.0
let count = 7
let median = total / count // expected-error {{binary operator '/' cannot be applied to operands of type 'Double' and 'Int'}} expected-note{{Overloads for '/' exist with these partially matching parameter lists: (Int, Int), (Double, Double)}}
