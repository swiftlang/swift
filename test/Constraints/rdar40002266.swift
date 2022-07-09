// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

struct S {
  init<T: NSNumber>(_ num: T) { // expected-note {{where 'T' = 'Bool'}}
    self.init(num != 0) // expected-error {{initializer 'init(_:)' requires that 'Bool' inherit from 'NSNumber'}}
    // expected-error@-1 {{cannot convert value of type 'T' to expected argument type 'Int'}}
  }
}
