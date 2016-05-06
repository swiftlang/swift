// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

#if os(OSX)
import AppKit
#endif

#if os(iOS) || os(tvOS) || os(watchOS)
import UIKit
#endif

extension CGColorSpace {
  class func deviceRGB() -> CGColorSpace {
    return CGColorSpaceCreateDeviceRGB()!
  }
}

extension CGColor {
  class func create(colorSpace colorSpace: CGColorSpace, components: [CGFloat])
      -> CGColor {
    return CGColor(colorSpace: colorSpace, components: components)!
  }

  var r: CGFloat { return components![0] }
  var g: CGFloat { return components![1] }
  var b: CGFloat { return components![2] }
}

let pink = CGColor.create(colorSpace: .deviceRGB(),
                          components: [1.0, 0.5, 0.25, 1.0])

// CHECK: 1.0
print(pink.r)
// CHECK-NEXT: 0.5
print(pink.g)
// CHECK-NEXT: 0.25
print(pink.b)

