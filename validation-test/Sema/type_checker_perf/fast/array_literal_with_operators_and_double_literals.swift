// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -solver-expression-time-threshold=1

// REQUIRES: asserts,no_asan
// REQUIRES: objc_interop

// FIXME: This should be a scale-test but it doesn't allow passing `%clang-importer-sdk`

// This import is important because it brings CGFloat and
// enables Double<->CGFloat implicit conversion that affects
// literals below.
import Foundation

let p/*: [(String, Bool, Bool, Double)]*/ = [
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0),
  ("", true, true, 0 * 0.0 * 0.0)
]
