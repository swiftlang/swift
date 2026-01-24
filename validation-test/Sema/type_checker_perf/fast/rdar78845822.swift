// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// REQUIRES: objc_interop

import Foundation

let y: TimeInterval = (5 * 60 * 60) + (4 * 60) + 3 + 0.625
