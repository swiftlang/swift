// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck %s -verify

import ObjectiveC

func instanceMethod(_ b: B) {
  b.method(1, 2.5) // expected-error {{argument labels '(_:, _:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'method' exist with these partially matching parameter lists: (Int32, onCat1: Double), (Int32, onExtA: Double), (Int32, onExtB: Double), (Int32, with: Double), (Int32, with: Float), (Int32, withDouble: Double), (Int32, withFloat: Float)}}
}
