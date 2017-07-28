// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -o - -primary-file %s -swift-version 4

// REQUIRES: objc_interop

import Foundation

class SwiftLaundryService : NSLaundry {
  var g: (Garment & Coat)? = nil

  func wash(_ g: Garment & Coat) {
    self.g = g
  }

  func bleach(_ g: Garment & Coat & Cotton) {}

  func dry() -> Garment & Coat {
    return g!
  }
}

// FIXME: Consider better diagnostics here.

class OldSwiftLaundryService : NSLaundry {
// expected-error@-1 {{type 'OldSwiftLaundryService' does not conform to protocol 'NSLaundry'}}

  var g: Coat? = nil

  func wash(_ g: Coat) { // expected-note {{candidate has non-matching type '(Coat) -> ()'}}
    self.g = g
  }

  func bleach(_ g: Coat) {} // expected-note {{candidate has non-matching type '(Coat) -> ()'}}

  func dry() -> Coat { // expected-note {{candidate has non-matching type '() -> Coat'}}
    return g!
  }
}

// Make sure the method lookup is not ambiguous

_ = Coat.fashionStatement.wear()
