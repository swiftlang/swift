// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck %s -verify

import ObjectiveC

func instanceMethod(_ b: B) {
  // Notes for labeling mismatch candidates are now attached to each individual declaration
  b.method(1, 2.5) // expected-error {{no exact matches in call to instance method 'method'}}
}
