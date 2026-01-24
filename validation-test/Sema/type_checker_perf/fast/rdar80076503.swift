// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// REQUIRES: objc_interop

import CoreGraphics

var touch1, touch2: CGPoint?

func slow() {
  if fabs(Float((touch1?.x ?? 0.0) - (touch2?.x ?? 0.0))) > fabs(Float((touch1?.y ?? 0.0) - (touch2?.y ?? 0.0))) {}
}
