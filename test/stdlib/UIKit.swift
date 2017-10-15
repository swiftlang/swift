// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=ios

import UIKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

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

UIKitTests.test("NSValue bridging") {
  expectBridgeToNSValue(UIEdgeInsets(top: 17, left: 38, bottom: 6, right: 79),
                        nsValueInitializer: { NSValue(uiEdgeInsets: $0) },
                        nsValueGetter: { $0.uiEdgeInsetsValue },
                        equal: (==))
  expectBridgeToNSValue(UIOffset(horizontal: 17, vertical: 38),
                        nsValueInitializer: { NSValue(uiOffset: $0) },
                        nsValueGetter: { $0.uiOffsetValue },
                        equal: (==))
}

#if os(iOS) || os(tvOS)
UIKitTests.test("UIContentSizeCategory comparison") {
    if #available(iOS 11.0, tvOS 11.0, *) {
        expectTrue(UIContentSizeCategory.large < UIContentSizeCategory.extraLarge)
        expectTrue(UIContentSizeCategory.large <= UIContentSizeCategory.extraLarge)
        expectFalse(UIContentSizeCategory.large >= UIContentSizeCategory.extraLarge)
        expectFalse(UIContentSizeCategory.large > UIContentSizeCategory.extraLarge)
        expectFalse(UIContentSizeCategory.large == UIContentSizeCategory.extraLarge)
        
        expectTrue(UIContentSizeCategory.extraLarge > UIContentSizeCategory.large)
        expectTrue(UIContentSizeCategory.extraLarge >= UIContentSizeCategory.large)
        expectFalse(UIContentSizeCategory.extraLarge < UIContentSizeCategory.large)
        expectFalse(UIContentSizeCategory.extraLarge <= UIContentSizeCategory.large)
        expectFalse(UIContentSizeCategory.extraLarge == UIContentSizeCategory.large)
        
        expectTrue(UIContentSizeCategory.large == UIContentSizeCategory.large)
        expectTrue(UIContentSizeCategory.large >= UIContentSizeCategory.large)
        expectTrue(UIContentSizeCategory.large <= UIContentSizeCategory.large)
        expectFalse(UIContentSizeCategory.large > UIContentSizeCategory.large)
        expectFalse(UIContentSizeCategory.large < UIContentSizeCategory.large)
        
        expectTrue(UIContentSizeCategory.accessibilityExtraExtraExtraLarge.isAccessibilityCategory)
        expectFalse(UIContentSizeCategory.extraSmall.isAccessibilityCategory)
    }
}
#endif

#if os(iOS) || os(watchOS) || os(tvOS)
UIKitTests.test("UIFontMetrics scaling") {
    if #available(iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
        let metrics = UIFontTextStyle.headline.metrics
        expectTrue(metrics != nil)
    }
}
#endif

#if os(iOS) || os(tvOS)
UIKitTests.test("UIFocusEnvironment") {
  if #available(iOS 11.0, tvOS 11.0, *) {
    let item1 = UIView()
    let item2 = UIView()
    _ = item1.contains(item2)
    _ = item1.isFocused
  }
}
#endif

#if os(iOS)

UIKitTests.test("NSItemProviderReadingWriting support") {
  if #available(iOS 11.0, *) {
    func f<T : UIDragDropSession>(session: T) {
      _ = session.canLoadObjects(ofClass: String.self)
      _ = session.canLoadObjects(ofClass: URL.self)
    }
    func g<T : UIDropSession>(session: T) {
      _ = session.loadObjects(ofClass: String.self) { _ in ()}
      _ = session.loadObjects(ofClass: URL.self) { _ in () }
    }

    let pc0 = UIPasteConfiguration(forAccepting: String.self)
    let pc1 = UIPasteConfiguration(forAccepting: URL.self)
    pc0.addTypeIdentifiers(forAccepting: URL.self)
    pc1.addTypeIdentifiers(forAccepting: String.self)

    var pb = UIPasteboard.general
    pb.setObjects(["Hello"])
    pb.setObjects([URL(string: "https://www.apple.com")!])
    pb.setObjects(["Hello"], localOnly: true, expirationDate: nil)
    pb.setObjects([URL(string: "https://www.apple.com")!], localOnly: true, expirationDate: nil)
  }
}

#endif


runAllTests()
