// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

#if os(macOS)
import AppKit
#endif
#if os(iOS) || os(tvOS) || os(watchOS)
import UIKit
#endif

let foo: [CGColor] =
  [CGColor(colorSpace: CGColorSpaceCreateDeviceRGB(),
	         components: [1.0, 0.0, 0.0, 1.0])!]

let bar = foo as NSArray

// CHECK: CGColor
print(bar[0])
