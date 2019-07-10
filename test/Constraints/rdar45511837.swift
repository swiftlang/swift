// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

protocol A: RawRepresentable {}

extension A {
  static func +(lhs: RawValue, rhs: Self) -> Self {
    fatalError()
  }
}

class Foo<Bar: NSObject> {
  var foobar: Bar {
    fatalError()
  }

  lazy var foo: () -> Void = {
    // TODO: improve diagnostic message
    _ = self.foobar + nil // expected-error {{'Foo<Bar>' requires that 'Bar' inherit from 'NSObject'}}
  }
}
