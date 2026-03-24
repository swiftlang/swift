// RUN: %target-swift-frontend %s -typecheck -solver-scope-threshold=300

// REQUIRES: objc_interop

// FIXME: This should be a scale-test but it doesn't allow importing SDK frameworks.

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
