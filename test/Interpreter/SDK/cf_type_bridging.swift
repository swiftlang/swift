// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

#if os(OSX)
import AppKit
#endif
#if os(iOS)
import UIKit
#endif

let foo: [CGColor] = [CGColorCreate(CGColorSpaceCreateDeviceRGB(), [1.0, 0.0, 0.0, 1.0])]

let bar = foo as NSArray

// CHECK: CGColor
println(bar[0])
