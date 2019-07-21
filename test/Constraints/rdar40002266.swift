// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

struct S {
  init<T: NSNumber>(_ num: T) {
    self.init(num != 0) // expected-error {{binary operator '!=' cannot be applied to operands of type 'T' and 'Int'}}
    // expected-note@-1 {{expected an argument list of type '(Self, Self)'}}
  }
}
