// RUN: %target-swift-frontend -parse -verify %s -enable-id-as-any
// REQUIRES: objc_interop

import Foundation

@objc var foo: Any // expected-error {{@objc can only be used with members of classes, @objc protocols, and concrete extensions of classes}}

class Foo: NSObject {
  override init() {}

  @objc var property: Any

  @objc func method(x: Any) -> Any { return x }

  @objc func indirectAny(x: UnsafePointer<Any>) {} // expected-error{{type of the parameter cannot be represented in Objective-C}}
}
