// RUN: %empty-directory(%t/cache)
// RUN: %target-run-simple-swift(-module-cache-path %t/cache)
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

#if canImport(AppKit)
import AppKit
#elseif canImport(UIKit)
import UIKit
#else
#error("Unsupported platform")
#endif

extension CGColorSpace {
  class func deviceRGB() -> CGColorSpace {
    return CGColorSpaceCreateDeviceRGB()
  }
}

extension CGColor {
  class func create(colorSpace: CGColorSpace, components: [CGFloat])
      -> CGColor {
    return CGColor(colorSpace: colorSpace, components: components)!
  }

  var r: CGFloat { return components![0] }
  var g: CGFloat { return components![1] }
  var b: CGFloat { return components![2] }
}

var CFTestSuite = TestSuite("CFExtensions")

CFTestSuite.test("methods") {
  let pink = CGColor.create(colorSpace: .deviceRGB(),
                            components: [1.0, 0.5, 0.25, 1.0])
  expectEqual(1.0, pink.r)
  expectEqual(0.5, pink.g)
  expectEqual(0.25, pink.b)
}

protocol SwiftProto {
  func doTheThing() -> AnyObject
}
extension CGColor: SwiftProto {
  func doTheThing() -> AnyObject { return self }
}

func callTheThing<T: SwiftProto>(_ instance: T) -> AnyObject {
  return instance.doTheThing()
}

CFTestSuite.test("protocols") {
  let pink = CGColor.create(colorSpace: .deviceRGB(),
                            components: [1.0, 0.5, 0.25, 1.0])
  expectTrue(pink === pink.doTheThing())

  let protoObj: SwiftProto = pink
  expectTrue(pink === protoObj.doTheThing())

  expectTrue(pink === callTheThing(pink))
}

CFTestSuite.test("protocols/downcast")
    .xfail(.always("unimplemented"))
    .code {
  let pink = CGColor.create(colorSpace: .deviceRGB(),
                            components: [1.0, 0.5, 0.25, 1.0])
  let opaquePink: AnyObject = pink
  let downcasted = opaquePink as? SwiftProto
  expectNotNil(downcasted)
  expectTrue(pink === downcasted!.doTheThing())
}

runAllTests()
