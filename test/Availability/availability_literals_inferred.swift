// RUN: %target-typecheck-verify-swift -swift-version 5

// https://github.com/apple/swift/issues/61890

@available(*, unavailable) // expected-note{{'S' has been explicitly marked unavailable here}}
struct S: ExpressibleByStringLiteral {
  init(stringLiteral value: String) {}
}
@available(*, unavailable)
typealias StringLiteralType = S

let i = "" // expected-error{{'S' is unavailable}}

@available(*, unavailable) // expected-note{{'S1' has been explicitly marked unavailable here}}
struct S1<T>: ExpressibleByIntegerLiteral {
  init(integerLiteral value: Int) {}
}
@available(*, unavailable)
typealias IntegerLiteralType = S1<Int>

let a = 0 // expected-error{{'S1' is unavailable}}
