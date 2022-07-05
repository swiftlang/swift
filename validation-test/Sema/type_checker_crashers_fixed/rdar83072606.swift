// RUN: %target-typecheck-verify-swift

struct A {
  var prop: Bool

  func test() {
    _ = AssertUnwrap(prop) as! Bool! // expected-warning {{using '!' here is deprecated and will be removed in a future release}}
    // expected-warning@-1 {{forced cast from 'Bool?' to 'Bool' only unwraps optionals; did you mean to use '!'?}}
  }

  @discardableResult
  func AssertUnwrap<T>(_ optional: T?) -> T? {
    return optional
  }
}
