// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -enable-parser-lookup

// REQUIRES: objc_interop

import Foundation

class MyClass: NSObject {
  func f() {
    let url = URL(url) // expected-error{{variable used within its own initial value}}
  }
}
