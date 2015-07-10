// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import ObjectiveC

func instanceMethod(b: B) {
  b.method(1, 2.5) // expected-error {{cannot invoke 'method' with an argument list of type '(Int, Double)'}}
  // expected-note @-1 {{expected an argument list of type '(Int32, onExtB: Double)'}}
}
