// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import CoreGraphics
import StdlibUnittest

//===----------------------------------------------------------------------===//
// CGColor
//===----------------------------------------------------------------------===//

// CGColor.components
let red = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
let components = red.components!
expectEqual(components.count, 4)
expectEqual(components[0], 1)
expectEqual(components[1], 0)
expectEqual(components[2], 0)
expectEqual(components[3], 1)

// CGColor Equatable conformance via CGColorEqualToColor
let aka = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
expectEqual(red, aka)

