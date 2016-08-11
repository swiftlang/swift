// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// <rdar://problem/17014037>
// REQUIRES: OS=macosx

import QuartzCore

class Canary: NSObject {
  deinit {
    print("died")
  }
}

var CanaryAssocObjectHandle: UInt8 = 0

// Attach an associated object with a loud deinit so we can see that the
// error died.
func hangCanary(_ o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, Canary(),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}

class FooLayer: CALayer {
  var black: CGColor
  var white: CGColor = CGColor.white

  override init() {
    black = CGColor.black
    super.init()
    hangCanary(self)
  }

  required init?(coder: NSCoder) {
    black = coder.decodeObject(forKey: "black") as! CGColor
    super.init(coder: coder)
  }

  override var description: String {
    return "FooLayer"
  }
}

do {
  let layer = FooLayer()
  print("\(layer)")
}

// CHECK: FooLayer
// CHECK: died
