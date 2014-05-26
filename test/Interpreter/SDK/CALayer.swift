// RUN: %target-run-simple-swift | FileCheck %s
// <rdar://problem/17014037>

import QuartzCore

class Canary: NSObject {
  deinit {
    println("died")
  }
}

var CanaryAssocObjectHandle: UInt8 = 0

// Attach an associated object with a loud deinit so we can see that the
// error died.
func hangCanary(o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                     objc_AssociationPolicy(OBJC_ASSOCIATION_RETAIN_NONATOMIC))
}

class FooLayer: CALayer {
  var black: CGColor
  var white: CGColor = CGColorGetConstantColor(kCGColorWhite)

  init() {
    black = CGColorGetConstantColor(kCGColorBlack)
    super.init()
    hangCanary(self)
  }

  override var description: String {
    return "FooLayer"
  }
}

if true {
  let layer = FooLayer()
  println("\(layer)")
}

// CHECK: FooLayer
// CHECK: died
