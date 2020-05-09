// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify
// REQUIRES: objc_interop

import Foundation

@objc class Foo: NSObject {
  let x: Int = 0
}
@objc protocol Bar {
  @objc optional var foo: Foo {get}
}

func baz(bar: Bar) {
  max(bar, bar.foo?.x ?? 0)
  // expected-error@-1 {{cannot convert value of type 'Bar' to expected argument type 'Int'}}
}
