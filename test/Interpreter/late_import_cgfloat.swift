// RUN: %target-repl-run-simple-swift | %FileCheck %s

// REQUIRES: swift_repl

// This used to crash.

let str = ""
import Foundation
let pt = CGPoint(x: 1.0, y: 2.0)
// CHECK: pt : CGPoint = (1.0, 2.0)

import simd
let f = float2(x: 1.0, y: 2.0)
// CHECK: f : float2 = SIMD2<Float>(1.0, 2.0)
