// RUN: %target-parse-verify-swift

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

  init(booleanLiteral value: Bool) {
    self.value = value
  }
}

func customBoolLiterals() {
  var b: CustomBool = false
  b = true
}

