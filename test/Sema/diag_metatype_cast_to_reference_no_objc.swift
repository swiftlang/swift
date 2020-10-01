// RUN: %target-swift-frontend -disable-objc-interop -typecheck %s -verify

class C {}

func test(c: AnyClass) {
  let _: AnyObject = c // expected-error {{value of type 'AnyClass' (aka 'AnyObject.Type') expected to be instance of class or class-constrained type}}
  let _: AnyObject = C.self // expected-error {{value of type 'C.Type' expected to be instance of class or class-constrained type}}
}
