// RUN: %target-typecheck-verify-swift

func boolLiterals() {
  var b: Bool = false
  b = true
  _ = b
}

func defaultBoolLiterals() {
  let b = false
  var _: Bool = b
}

struct CustomBool : ExpressibleByBooleanLiteral {
  let value: Bool

  init(booleanLiteral value: Bool) {
    self.value = value
  }
}

func customBoolLiterals() {
  var b: CustomBool = false
  b = true
  _ = b
}

