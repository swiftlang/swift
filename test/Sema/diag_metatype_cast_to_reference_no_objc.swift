// RUN: %target-swift-frontend -disable-objc-interop -parse %s -verify

class C {}

func test(c: AnyClass) {
  let _: AnyObject = c // expected-error {{type 'AnyClass' does not conform to protocol 'AnyObject'}}
  let _: AnyObject = C.self // expected-error {{type 'C.Type' does not conform to protocol 'AnyObject'}}
}
