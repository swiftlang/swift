// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify
// REQUIRES: objc_interop

import Foundation

struct S {
  var test: CGFloat
}

func compute(_: Double?) -> Double? {
  return 42
}

func test(s: S?) {
  _ = compute(s?.test) // expected-error {{cannot implicitly convert value of type 'CGFloat?' to expected type 'Double?'}}
}
