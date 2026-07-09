// RUN: %target-swift-frontend %s -typecheck -verify
// REQUIRES: objc_interop

import Foundation

struct S {
  var test: CGFloat
}

func compute(_: Double?) -> Double? {
  return 42
}

func test(s: S?) {
  _ = compute(s?.test)
}
