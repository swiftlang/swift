// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

// QuartzCore is not present on watchOS.
// UNSUPPORTED: OS=watchos

import QuartzCore
import StdlibUnittest
import StdlibUnittestFoundationExtras

var CanaryAssocObjectHandle: UInt8 = 0

// Attach a LifetimeTracked associated object to an object so we can see that
// the object died.
func hangCanary(_ o: AnyObject) {
  objc_setAssociatedObject(o, &CanaryAssocObjectHandle, LifetimeTracked(0),
                           .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
}

let deviceRGB = CGColorSpaceCreateDeviceRGB()

class FooLayer: CALayer {
  var black: CGColor 
  var white = CGColor(colorSpace: deviceRGB, components: [1,1,1,1])!

  override init() {
    black = CGColor(colorSpace: deviceRGB, components: [0,0,0,1])!
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

let quartzCore = TestSuite("QuartzCore")

quartzCore.test("CALayer retain/release through subclass initializers") {
  let layer = FooLayer()
  expectEqual("FooLayer", "\(layer)")
}

func equalCATransform3D(_ x: CATransform3D, _ y: CATransform3D) -> Bool {
  var xx = x, yy = y
  return memcmp(&xx, &yy, MemoryLayout<CATransform3D>.size) == 0
}

quartzCore.test("CATransform3D bridges to NSValue") {
  expectBridgeToNSValue(CATransform3DMakeRotation(.pi, 1, 0, 0),
                        nsValueInitializer: { NSValue(caTransform3D: $0) },
                        nsValueGetter: { $0.caTransform3DValue },
                        equal: equalCATransform3D)
}

runAllTests()

