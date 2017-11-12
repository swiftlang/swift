// RUN: %target-swift-frontend -disable-objc-interop -typecheck %s -verify

class C {}

func test(c: AnyClass) {
  let _: AnyObject = c // expected-error {{value of type 'AnyClass' (aka 'AnyObject.Type') does not conform to specified type 'AnyObject'}}
  let _: AnyObject = C.self // expected-error {{value of type 'C.Type' does not conform to specified type 'AnyObject'}}
}
