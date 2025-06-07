// RUN: %target-typecheck-verify-swift

struct A {
  var prop: Bool

  func test() {
    _ = AssertUnwrap(prop) as! Bool! // expected-warning {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}
    // expected-warning@-1 {{forced cast from 'Bool?' to 'Bool' only unwraps optionals; did you mean to use '!'?}}
  }

  @discardableResult
  func AssertUnwrap<T>(_ optional: T?) -> T? {
    return optional
  }
}
