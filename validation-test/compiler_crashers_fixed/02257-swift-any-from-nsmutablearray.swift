// RUN: %target-swift-frontend %s -emit-ir

// REQUIRES: objc_interop

// Issue found by https://github.com/mattdaw (Matt Daw)

import Foundation
var a: NSMutableArray = [""]
var b: Any = a[0]
