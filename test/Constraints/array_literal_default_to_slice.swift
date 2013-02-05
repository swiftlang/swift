// RUN: %swift -parse -verify -constraint-checker %s
// XFAIL: *

protocol ArrayLiteralConvertible {
  typealias Element
  static func convertFromArrayLiteral(xs:Element...) -> This
}

var a = [1,2,3]
var a2 : Int[] = a

var b = [1,2,3.0]
var b2 : Double[] = b

