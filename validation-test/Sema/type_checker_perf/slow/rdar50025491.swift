// RUN: not %target-typecheck-verify-swift -solver-scope-threshold=1000

import Foundation

// REQUIRES: objc_interop

var x: CGFloat! = 0
var y: CGFloat! = 0
let z: CGFloat = 0
let w: CGFloat! = x/2 - z/2 - y/2
