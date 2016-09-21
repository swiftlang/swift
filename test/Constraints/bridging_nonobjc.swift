// RUN: %target-swift-frontend -parse -verify %s -disable-objc-interop


var x: Any = 1
var y = x as AnyObject // expected-error{{not convertible}}
