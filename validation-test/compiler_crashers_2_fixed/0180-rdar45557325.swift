// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -disable-parser-lookup

// REQUIRES: objc_interop

import Foundation

class MyClass: NSObject {
  func f() {
    let url = URL(url) // expected-error{{use of local variable 'url' before its declaration}}
    // expected-note@-1 {{'url' declared here}}
  }
}
