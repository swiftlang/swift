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
    _ = self.foobar + nil // expected-error {{'Bar' is not convertible to 'String'; did you mean to use 'as!' to force downcast?}}
    // expected-error@-1 {{'nil' is not compatible with expected argument type 'String'}}
  }
}
