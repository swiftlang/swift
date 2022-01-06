// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink
// REQUIRES: OS=macosx,tools-release,no_asan

import Foundation

var r: Float  = 0
var x: Double = 0
var y: Double = 0

let _ = (1.0 - 1.0 / (1.0 + exp(-5.0 * (r - 0.05)/0.01))) * Float(x) + Float(y)
