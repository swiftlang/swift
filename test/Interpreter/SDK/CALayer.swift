// RUN: %target-run-simple-swift | FileCheck %s
// <rdar://problem/17014037>
// REQUIRES: OS=macosx

import QuartzCore

class Canary: NSObject {
  deinit {
    println("died")
  }
}

var CanaryAssocObjectHandle: UInt8 = 0

// Hack to build with both older and newer SDKs.
// rdar://problem/19494514
extension UInt {
  static let OBJC_ASSOCIATION_RETAIN_NONATOMIC: UInt = 1
}

// Attach an associated object with a loud deinit so we can see that the
// error died.
func hangCanary(o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}

class FooLayer: CALayer {
  var black: CGColor
  var white: CGColor = CGColorGetConstantColor(kCGColorWhite)

  override init() {
    black = CGColorGetConstantColor(kCGColorBlack)
    super.init()
    hangCanary(self)
  }

  required init?(coder: NSCoder) {
    black = coder.decodeObjectForKey("black") as! CGColor
    super.init(coder: coder)
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
