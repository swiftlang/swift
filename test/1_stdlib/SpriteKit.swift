// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

// watchOS does not have SpriteKit.
// UNSUPPORTED: OS=watchos

import Foundation
import SpriteKit

// Check that the subscript is there.
@available(OSX,introduced: 10.10)
@available(iOS,introduced: 8.0)
@available(tvOS,introduced: 8.0)
@available(watchOS,introduced: 2.0)
func testSubscript(_ node: SKNode) {
  var _: [SKNode] = node["me"]
}

// SKColor is NSColor on OS X and UIColor on iOS.

var r = CGFloat(0)
var g = CGFloat(0)
var b = CGFloat(0)
var a = CGFloat(0)
var color = SKColor.red()
color.getRed(&r, green:&g, blue:&b, alpha:&a)
print("color \(r) \(g) \(b) \(a)")
// CHECK: color 1.0 0.0 0.0 1.0

#if os(OSX)
func f(_ c: NSColor) {
  print("colortastic")
}
#endif
#if os(iOS) || os(tvOS)
func f(_ c: UIColor) {
  print("colortastic")
}
#endif
f(color)
// CHECK: colortastic
