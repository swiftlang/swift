// RUN: not --crash %target-swift-frontend %s -parse

struct rdar27680407 : ExpressibleByStringLiteral {
  let value: String

  // Stack overflow while validating rdar27680407.StringLiteralType.
  init(stringLiteral value: rdar27680407.StringLiteralType) {
    self.value = value
  }
}
