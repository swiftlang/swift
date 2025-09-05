// RUN: %empty-directory(%t/cache)
// RUN: %target-run-simple-swift(-module-cache-path %t/cache)
// REQUIRES: executable_test

// REQUIRES: objc_interop

#if canImport(AppKit)
import AppKit
#elseif canImport(UIKit)
import UIKit
#else
#error("Unsupported platform")
#endif

let foo: [CGColor] =
  [CGColor(colorSpace: CGColorSpaceCreateDeviceRGB(),
	         components: [1.0, 0.0, 0.0, 1.0])!]

let bar = foo as NSArray

// CHECK: CGColor
print(bar[0])
