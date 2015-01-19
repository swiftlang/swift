// RUN: %target-swift-frontend -disable-objc-interop -parse %s -verify

func test(c: AnyClass) {
  var obj: AnyObject = c // expected-error {{converting a metatype to a reference is only supported with an Objective-C runtime}}
}
