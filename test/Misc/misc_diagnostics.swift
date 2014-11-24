// RUN: %target-swift-frontend %s -parse -verify
// XFAIL: linux
import Foundation
import CoreGraphics

var roomName : String? = nil

if let realRoomName = roomName as NSString { // expected-warning{{always fails}} expected-error{{use 'as?'}}
			
}

var pi = 3.14159265358979
var d: CGFloat = 2.0
var dpi:CGFloat = d*pi // expected-error{{cannot invoke '*' with an argument list of type '(@lvalue CGFloat, @lvalue Double)'}}

let ff: CGFloat = floorf(20.0) // expected-error{{'Float' is not convertible to 'CGFloat'}}

let total = 15.0
let count = 7
let median = total / count // expected-error {{cannot invoke '/' with an argument list of type '(Double, Int)'}}
