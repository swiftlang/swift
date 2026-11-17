// RUN: %target-typecheck-verify-swift -language-mode 4 -verify-additional-prefix swift4-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift%language_mode-

struct A {
  var prop: Bool

  func test() {
    _ = AssertUnwrap(prop) as! Bool! // expected-swift4-warning {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
    // expected-swift5-error@-1 {{using '!' is not allowed here}}
    // expected-warning@-2 {{forced cast from 'Bool?' to 'Bool' only unwraps optionals; did you mean to use '!'?}}
  }

  @discardableResult
  func AssertUnwrap<T>(_ optional: T?) -> T? {
    return optional
  }
}
