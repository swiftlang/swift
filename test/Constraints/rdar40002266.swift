// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

struct S {
  init<T: NSNumber>(_ num: T) {
    self.init(num != 0) // expected-error {{cannot convert value of type 'T' to expected argument type 'Int'}}
  }
}
