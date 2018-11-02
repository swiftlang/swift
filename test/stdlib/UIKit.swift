// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 3 %s -o %t/a.out3 && %target-run %t/a.out3
// RUN: %target-build-swift -swift-version 4 %s -o %t/a.out4 && %target-run %t/a.out4
// REQUIRES: executable_test
// UNSUPPORTED: OS=macosx
// REQUIRES: objc_interop

import UIKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

#if swift(>=4)
  let UIKitTests = TestSuite("UIKit_Swift4")
#else
  let UIKitTests = TestSuite("UIKit_Swift3")
#endif

#if !os(watchOS) && !os(tvOS)
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
#endif

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

UIKitTests.test("UIFont.Weight") {
  guard #available(iOS 8.2, *) else { return }
  #if swift(>=4) // Swift 4
    let regularFontWeight: UIFont.Weight = .regular

    expectTrue(regularFontWeight == .regular)
    expectTrue(regularFontWeight > .light)
    expectTrue(regularFontWeight < .heavy)
    expectTrue(regularFontWeight + 0.1 == 0.1 + regularFontWeight)
  #else // Swift 3
    let regularFontWeight: UIFontWeight = UIFontWeightRegular

    expectTrue(regularFontWeight == UIFontWeightRegular)
    expectTrue(regularFontWeight > UIFontWeightLight)
    expectTrue(regularFontWeight < UIFontWeightHeavy)
    expectTrue(regularFontWeight + 0.1 == 0.1 + UIFontWeightRegular)
  #endif
}

#if !os(watchOS)
UIKitTests.test("UILayoutPriority") {
  #if swift(>=4) // Swift 4
    let lowLayoutPriority: UILayoutPriority = .defaultLow
    let highLayoutPriority: UILayoutPriority = .defaultHigh

    expectTrue(lowLayoutPriority < highLayoutPriority)

    expectTrue(lowLayoutPriority + 2.0 == UILayoutPriority(lowLayoutPriority.rawValue + 2.0))
    expectTrue(2.0 + lowLayoutPriority == UILayoutPriority(lowLayoutPriority.rawValue + 2.0))
    expectTrue(lowLayoutPriority - 2.0 == UILayoutPriority(lowLayoutPriority.rawValue - 2.0))
    expectTrue(highLayoutPriority - lowLayoutPriority == highLayoutPriority.rawValue - lowLayoutPriority.rawValue)

    expectTrue(lowLayoutPriority + (highLayoutPriority - lowLayoutPriority) == highLayoutPriority)

    var mutablePriority = lowLayoutPriority
    mutablePriority -= 1.0
    mutablePriority += 2.0
    expectTrue(mutablePriority == lowLayoutPriority + 1.0)

    let priorotyRange = lowLayoutPriority...highLayoutPriority
    expectTrue(priorotyRange.contains(.defaultLow))
    expectFalse(priorotyRange.contains(.required))
  #else // Swift 3
    let lowLayoutPriority: UILayoutPriority = UILayoutPriorityDefaultLow
    let highLayoutPriority: UILayoutPriority = UILayoutPriorityDefaultHigh

    expectTrue(lowLayoutPriority < highLayoutPriority)

    expectTrue(2.0 + lowLayoutPriority == lowLayoutPriority + 2.0)
    expectTrue(lowLayoutPriority + (highLayoutPriority - lowLayoutPriority) == highLayoutPriority)

    var mutablePriority = lowLayoutPriority
    mutablePriority -= 1.0
    mutablePriority += 2.0
    expectTrue(mutablePriority == lowLayoutPriority + 1.0)
  #endif
}
#endif

#if !os(watchOS)
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
#endif

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
