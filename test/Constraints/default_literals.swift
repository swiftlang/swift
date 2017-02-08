// RUN: %target-typecheck-verify-swift

func acceptInt(_ : inout Int) {}
func acceptDouble(_ : inout Double) {}

var i1 = 1
acceptInt(&i1)
var i2 = 1 + 2.0 + 1
acceptDouble(&i2)


func ternary<T>(_ cond: Bool,
                _ ifTrue: @autoclosure () -> T,
                _ ifFalse: @autoclosure () -> T) -> T {}
_ = ternary(false, 1, 2.5)
_ = ternary(false, 2.5, 1)

// <rdar://problem/18447543>
_ = ternary(false, 1, 2 as Int32)
_ = ternary(false, 1, 2 as Float)

func genericFloatingLiteral<T : ExpressibleByFloatLiteral>(_ x: T) {
  var _ : T = 2.5
}

var d = 3.5
genericFloatingLiteral(d)

extension UInt32 {
  func asChar() -> UnicodeScalar { return UnicodeScalar(self)! }
}
var ch = UInt32(65).asChar()

// <rdar://problem/14634379>
extension Int {
  func call0() {}
  typealias Signature = (a: String, b: String)
  func call(_ x: Signature) {}
}

3.call((a: "foo", b: "bar"))


var (div, mod) = (9 / 4, 9 % 4)
