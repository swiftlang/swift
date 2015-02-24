// RUN: %target-swift-frontend %s -emit-ir

// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/mattdaw (Matt Daw)

import Foundation
var a: NSMutableArray = [""]
var b: Any = a[0]
