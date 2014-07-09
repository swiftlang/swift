// RUN: %swift -parse %s -verify

func boolLiterals() {
  var b: Bool = false
  b = true
}

func defaultBoolLiterals() {
  var b = false
  var b2: Bool = b
}

struct CustomBool : BooleanLiteralConvertible {
  let value: Bool

  static func convertFromBooleanLiteral(value: Bool) -> CustomBool {
    return CustomBool(value: value)
  }
}

func customBoolLiterals() {
  var b: CustomBool = false
  b = true
}

