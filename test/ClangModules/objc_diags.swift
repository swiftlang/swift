// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import ObjectiveC

func instanceMethod(b: B) {
  b.method(1, 2.5) // expected-error {{cannot invoke 'method' with an argument list of type '(Int, Double)'}}
  // expected-note @-1 {{overloads for 'method' exist with these partially matching parameter lists: (Int32, onExtB: Double), (Int32, onExtA: Double), (Int32, onCat1: Double), (Int32, withDouble: Double)}}
}
