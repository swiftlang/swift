// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: objc_interop

import Foundation

func slow() {
  let _ = { (elapsed: TimeInterval, duration: TimeInterval) -> Double in
    let s: TimeInterval = 1.70158
    var position: TimeInterval = elapsed / duration
    position -= 1.0
    return Double( position * position * ((s + 1.0) * position + s) + 1.0 )
  }
}
