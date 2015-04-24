// RUN: %target-parse-verify-swift

func acceptInt(inout _ : Int) {}
func acceptDouble(inout _ : Double) {}

var i1 = 1
acceptInt(&i1)
var i2 = 1 + 2.0 + 1
acceptDouble(&i2)


func ternary<T>(cond: Bool,
                @autoclosure _ ifTrue: () -> T,
                @autoclosure _ ifFalse: () -> T) -> T {}
ternary(false, 1, 2.5)
ternary(false, 2.5, 1)

// <rdar://problem/18447543>
ternary(false, 1, 2 as Int32)
ternary(false, 1, 2 as Float)

func genericFloatingLiteral<T : FloatLiteralConvertible>(x: T) {
  var x2 : T = 2.5
}

var d = 3.5
genericFloatingLiteral(d)

extension UInt32 {
  func asChar() -> UnicodeScalar { return UnicodeScalar(self) }
}
var ch = UInt32(65).asChar()

// <rdar://problem/14634379>
extension Int {
  func call0() {}
  typealias Signature = (a: String, b: String)
  func call(x: Signature) {}
}

3.call((a: "foo", b: "bar"))


var (div, mod) = (9 / 4, 9 % 4)
