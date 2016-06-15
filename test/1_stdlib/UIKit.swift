// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=ios

import UIKit
import StdlibUnittest

let UIKitTests = TestSuite("UIKit")

private func printDevice(_ o: UIDeviceOrientation) -> String {
  var s = "\(o.isPortrait) \(UIDeviceOrientationIsPortrait(o)), "
  s += "\(o.isLandscape) \(UIDeviceOrientationIsLandscape(o)), "
  s += "\(o.isFlat), \(o.isValidInterfaceOrientation) "
  s += "\(UIDeviceOrientationIsValidInterfaceOrientation(o))"
  return s
}

private func printInterface(_ o: UIInterfaceOrientation) -> String {
  return "\(o.isPortrait) \(UIInterfaceOrientationIsPortrait(o)), " +
    "\(o.isLandscape) \(UIInterfaceOrientationIsLandscape(o))"
}

UIKitTests.test("UIDeviceOrientation") {
  expectEqual("false false, false false, false, false false",
    printDevice(.unknown))

  expectEqual("true true, false false, false, true true",
    printDevice(.portrait))

  expectEqual("true true, false false, false, true true",
    printDevice(.portraitUpsideDown))

  expectEqual("false false, true true, false, true true",
    printDevice(.landscapeLeft))

  expectEqual("false false, true true, false, true true",
    printDevice(.landscapeRight))

  expectEqual("false false, false false, true, false false",
    printDevice(.faceUp))

  expectEqual("false false, false false, true, false false",
    printDevice(.faceDown))
}

UIKitTests.test("UIInterfaceOrientation") {
  expectEqual("false false, false false",
    printInterface(.unknown))

  expectEqual("true true, false false",
    printInterface(.portrait))

  expectEqual("true true, false false",
    printInterface(.portraitUpsideDown))

  expectEqual("false false, true true",
    printInterface(.landscapeLeft))

  expectEqual("false false, true true",
    printInterface(.landscapeRight))
}

UIKitTests.test("UIEdgeInsets") {
  let insets = [
    UIEdgeInsets(top: 1.0, left: 2.0, bottom: 3.0, right: 4.0),
    UIEdgeInsets(top: 1.0, left: 2.0, bottom: 3.1, right: 4.0),
    UIEdgeInsets.zero
  ]
  checkEquatable(insets, oracle: { $0 == $1 })
}

UIKitTests.test("UIOffset") {
  let offsets = [
    UIOffset(horizontal: 1.0, vertical: 2.0),
    UIOffset(horizontal: 1.0, vertical: 3.0),
    UIOffset.zero
  ]
  checkEquatable(offsets, oracle: { $0 == $1 })
}

class TestChildView : UIView, CustomPlaygroundQuickLookable {
  convenience init() {
    self.init(frame: CGRect(x: 0, y: 0, width: 10, height: 10))
  }
  var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text("child")
  }
}

UIKitTests.test("CustomPlaygroundQuickLookable") {
  switch PlaygroundQuickLook(reflecting: TestChildView()) {
  case .text("child"): break
  default: expectUnreachable(
    "TestChildView custom quicklookable should have been invoked")
  }
}

runAllTests()
