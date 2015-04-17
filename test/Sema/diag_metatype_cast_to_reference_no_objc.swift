// RUN: %target-swift-frontend -disable-objc-interop -parse %s -verify

class C {}

func test(c: AnyClass) {
  let obj: AnyObject = c // expected-error {{type 'AnyClass' does not conform to protocol 'AnyObject'}}
  let obj2: AnyObject = C.self // expected-error {{type 'C.Type' does not conform to protocol 'AnyObject'}}
}
