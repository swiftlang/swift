// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import SpriteKit

// SKColor is NSColor on OS X and UIColor on iOS.

var r = CGFloat(0)
var g = CGFloat(0)
var b = CGFloat(0)
var a = CGFloat(0)
var color = SKColor.redColor()
color.getRed(&r, green:&g, blue:&b, alpha:&a)
println("color \(r) \(g) \(b) \(a)")
// CHECK: color 1.0 0.0 0.0 1.0

#if os(OSX)
func f(c: NSColor) {
  println("colortastic")
}
#endif
#if os(iOS)
func f(c: UIColor) {
  println("colortastic")
}
#endif
f(color)
// CHECK: colortastic
