//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
@_exported import UIKit

#if os(iOS) || os(tvOS)
import _SwiftUIKitOverlayShims
#endif

//===----------------------------------------------------------------------===//
// UIGeometry
//===----------------------------------------------------------------------===//

extension UIEdgeInsets : Equatable {
  @_transparent // @fragile
  public static func == (lhs: UIEdgeInsets, rhs: UIEdgeInsets) -> Bool {
    return lhs.top == rhs.top &&
           lhs.left == rhs.left &&
           lhs.bottom == rhs.bottom &&
           lhs.right == rhs.right
  }
}

@available(iOS 11.0, tvOS 11.0, watchOS 4.0, *)
extension NSDirectionalEdgeInsets : Equatable {
  @_transparent // @fragile
  public static func == (lhs: NSDirectionalEdgeInsets, rhs: NSDirectionalEdgeInsets) -> Bool {
    return lhs.top == rhs.top &&
           lhs.leading == rhs.leading &&
           lhs.bottom == rhs.bottom &&
           lhs.trailing == rhs.trailing
  }
}

extension UIOffset : Equatable {
  @_transparent // @fragile
  public static func == (lhs: UIOffset, rhs: UIOffset) -> Bool {
    return lhs.horizontal == rhs.horizontal &&
           lhs.vertical == rhs.vertical
  }
}

#if os(iOS) || os(tvOS)
extension UIFloatRange : Equatable {
  @_transparent // @fragile
  public static func == (lhs: UIFloatRange, rhs: UIFloatRange) -> Bool {
    return lhs.minimum == rhs.minimum &&
           lhs.maximum == rhs.maximum
  }
}
#endif

@available(swift, deprecated: 4.2, message:"Use == operator instead.")
public func UIEdgeInsetsEqualToEdgeInsets(_ insets1: UIEdgeInsets, _ insets2: UIEdgeInsets) -> Bool {
  return insets1 == insets2
}

@available(swift, deprecated: 4.2, message:"Use == operator instead.")
public func UIOffsetEqualToOffset(_ offset1: UIOffset, _ offset2: UIOffset) -> Bool {
  return offset1 == offset2
}

#if os(iOS) || os(tvOS)
@available(swift, deprecated: 4.2, message:"Use == operator instead.")
public func UIFloatRangeIsEqualToRange(_ range: UIFloatRange, _ otherRange: UIFloatRange) -> Bool {
  return range == otherRange
}
#endif

//===----------------------------------------------------------------------===//
// Numeric backed types
//===----------------------------------------------------------------------===//

@available(swift 4)
public protocol _UIKitNumericRawRepresentable : RawRepresentable, Comparable where RawValue: Comparable & Numeric {}

extension _UIKitNumericRawRepresentable {

  public static func <(lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

  public static func +(lhs: Self, rhs: RawValue) -> Self {
    return Self(rawValue: lhs.rawValue + rhs)!
  }

  public static func +(lhs: RawValue, rhs: Self) -> Self {
    return Self(rawValue: lhs + rhs.rawValue)!
  }

  public static func -(lhs: Self, rhs: RawValue) -> Self {
    return Self(rawValue: lhs.rawValue - rhs)!
  }

  public static func -(lhs: Self, rhs: Self) -> RawValue {
    return lhs.rawValue - rhs.rawValue
  }

  public static func +=(lhs: inout Self, rhs: RawValue) {
    lhs = Self(rawValue: lhs.rawValue + rhs)!
  }

  public static func -=(lhs: inout Self, rhs: RawValue) {
    lhs = Self(rawValue: lhs.rawValue - rhs)!
  }
}

extension UIFont.Weight : _UIKitNumericRawRepresentable {}

#if !os(watchOS)
extension UILayoutPriority : _UIKitNumericRawRepresentable {}
#endif

// These are un-imported macros in UIKit.

//===----------------------------------------------------------------------===//
// UIDeviceOrientation
//===----------------------------------------------------------------------===//

#if !os(watchOS) && !os(tvOS)
@available(swift, obsoleted: 4.2,
  renamed: "getter:UIDeviceOrientation.isLandscape(self:)")
public func UIDeviceOrientationIsLandscape(
  _ orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isLandscape
}

@available(swift, obsoleted: 4.2,
  renamed: "getter:UIDeviceOrientation.isPortrait(self:)")
public func UIDeviceOrientationIsPortrait(
  _ orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isPortrait
}

@available(swift, obsoleted: 4.2,
  renamed: "getter:UIDeviceOrientation.isValidInterfaceOrientation(self:)")
public func UIDeviceOrientationIsValidInterfaceOrientation(
  _ orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isValidInterfaceOrientation
}
#endif

//===----------------------------------------------------------------------===//
// UIInterfaceOrientation
//===----------------------------------------------------------------------===//

#if !os(watchOS) && !os(tvOS)
@available(swift, obsoleted: 4.2,
  renamed: "getter:UIInterfaceOrientation.isPortrait(self:)")
public func UIInterfaceOrientationIsPortrait(
  _ orientation: UIInterfaceOrientation
) -> Bool {
  return orientation.isPortrait
}

@available(swift, obsoleted: 4.2,
  renamed: "getter:UIInterfaceOrientation.isLandscape(self:)")
public func UIInterfaceOrientationIsLandscape(
  _ orientation: UIInterfaceOrientation
) -> Bool {
  return orientation.isLandscape
}
#endif

// Overlays for variadic initializers.

#if !os(watchOS) && !os(tvOS)
public extension UIActionSheet {
  convenience init(title: String?,
       delegate: UIActionSheetDelegate?,
       cancelButtonTitle: String?,
       destructiveButtonTitle: String?,
       // Hack around overload ambiguity with non-variadic constructor.
       // <rdar://problem/16704770>
       otherButtonTitles firstButtonTitle: String,
       _ moreButtonTitles: String...) {
    self.init(title: title,
              delegate: delegate,
              cancelButtonTitle: cancelButtonTitle,
              destructiveButtonTitle: destructiveButtonTitle)
    self.addButton(withTitle: firstButtonTitle)
    for buttonTitle in moreButtonTitles {
      self.addButton(withTitle: buttonTitle)
    }
  }
}
#endif

#if !os(watchOS) && !os(tvOS)
public extension UIAlertView {
  convenience init(title: String,
       message: String,
       delegate: UIAlertViewDelegate?,
       cancelButtonTitle: String?,
       // Hack around overload ambiguity with non-variadic constructor.
       // <rdar://problem/16704770>
       otherButtonTitles firstButtonTitle: String,
       _ moreButtonTitles: String...) {
    self.init(title: title,
              message: message,
              delegate: delegate,
              cancelButtonTitle: cancelButtonTitle)
    self.addButton(withTitle: firstButtonTitle)
    for buttonTitle in moreButtonTitles {
      self.addButton(withTitle: buttonTitle)
    }
  }
}
#endif

#if !os(watchOS)
internal struct _UIViewQuickLookState {
  static var views = Set<UIView>()
}

extension UIView : _DefaultCustomPlaygroundQuickLookable {
  @available(*, deprecated, message: "UIView._defaultCustomPlaygroundQuickLook will be removed in a future Swift version")
  public var _defaultCustomPlaygroundQuickLook: PlaygroundQuickLook {
    if _UIViewQuickLookState.views.contains(self) {
      return .view(UIImage())
    } else {
      _UIViewQuickLookState.views.insert(self)
      // in case of an empty rectangle abort the logging
      if (bounds.size.width == 0) || (bounds.size.height == 0) {
        return .view(UIImage())
      }

      UIGraphicsBeginImageContextWithOptions(bounds.size, false, 0.0)
      // UIKit is about to update this to be optional, so make it work
      // with both older and newer SDKs. (In this context it should always
      // be present.)
      let ctx: CGContext! = UIGraphicsGetCurrentContext()
      UIColor(white:1.0, alpha:0.0).set()
      ctx.fill(bounds)
      layer.render(in: ctx)

      let image: UIImage! = UIGraphicsGetImageFromCurrentImageContext()

      UIGraphicsEndImageContext()

      _UIViewQuickLookState.views.remove(self)
      return .view(image)
    }
  }
}
#endif

extension UIColor : _ExpressibleByColorLiteral {
  @nonobjc public required convenience init(_colorLiteralRed red: Float,
                                            green: Float,
                                            blue: Float, alpha: Float) {
    self.init(red: CGFloat(red), green: CGFloat(green),
              blue: CGFloat(blue), alpha: CGFloat(alpha))
  }
}

public typealias _ColorLiteralType = UIColor

extension UIImage : _ExpressibleByImageLiteral {
  private convenience init!(failableImageLiteral name: String) {
    self.init(named: name)
  }

  public required convenience init(imageLiteralResourceName name: String) {
    self.init(failableImageLiteral: name)
  }
}

public typealias _ImageLiteralType = UIImage

extension UIFontTextStyle {
    @available(iOS 11.0, watchOS 4.0, tvOS 11.0, *)
    public var metrics: UIFontMetrics {
        return UIFontMetrics(forTextStyle: self)
    }
}

#if !os(watchOS) // UIContentSizeCategory not available on watchOS
extension UIContentSizeCategory {

    @available(iOS 11.0, tvOS 11.0,  *)
    public var isAccessibilityCategory: Bool {
        return __UIContentSizeCategoryIsAccessibilityCategory(self)
    }

    @available(iOS 11.0, tvOS 11.0, *)
    public static func < (left: UIContentSizeCategory, right: UIContentSizeCategory) -> Bool {
        return __UIContentSizeCategoryCompareToCategory(left, right) == .orderedAscending
    }

    @available(iOS 11.0, tvOS 11.0, *)
    public static func <= (left: UIContentSizeCategory, right: UIContentSizeCategory) -> Bool {
        return __UIContentSizeCategoryCompareToCategory(left, right) != .orderedDescending
    }

    @available(iOS 11.0, tvOS 11.0, *)
    public static func > (left: UIContentSizeCategory, right: UIContentSizeCategory) -> Bool {
        return __UIContentSizeCategoryCompareToCategory(left, right) == .orderedDescending
    }

    @available(iOS 11.0, tvOS 11.0, *)
    public static func >= (left: UIContentSizeCategory, right: UIContentSizeCategory) -> Bool {
        return __UIContentSizeCategoryCompareToCategory(left, right) != .orderedAscending
    }
}
#endif

//===----------------------------------------------------------------------===//
// Focus
//===----------------------------------------------------------------------===//

#if os(iOS) || os(tvOS)
@available(iOS 11.0, tvOS 11.0, *)
extension UIFocusEnvironment {
  @available(iOS 11.0, tvOS 11.0, *)
  public func contains(_ environment: UIFocusEnvironment) -> Bool {
    return _swift_UIKit_UIFocusEnvironmentContainsEnvironment(self, environment)
  }
}

@available(iOS 11.0, tvOS 11.0, *)
extension UIFocusItem {
  @available(iOS 11.0, tvOS 11.0, *)
  public var isFocused: Bool {
    return self === UIScreen.main.focusedItem
  }
}
#endif

//===----------------------------------------------------------------------===//
// NSItemProviderReading/Writing support
//===----------------------------------------------------------------------===//

#if os(iOS)

@available(iOS 11.0, *)
extension UIDragDropSession {
  @available(iOS 11.0, *)
  public func canLoadObjects<
    T : _ObjectiveCBridgeable
  >(ofClass: T.Type) -> Bool where T._ObjectiveCType : NSItemProviderReading {
    return self.canLoadObjects(ofClass: T._ObjectiveCType.self);
  }
}

@available(iOS 11.0, *)
extension UIDropSession {
  @available(iOS 11.0, *)
  public func loadObjects<
    T : _ObjectiveCBridgeable
  >(
    ofClass: T.Type,
    completion: @escaping ([T]) -> Void
  ) -> Progress where T._ObjectiveCType : NSItemProviderReading {
    return self.loadObjects(ofClass: T._ObjectiveCType.self) { nss in
      let natives = nss.map { $0 as! T }
      completion(natives)
    }
  }
}

@available(iOS 11.0, *)
extension UIPasteConfiguration {
  @available(iOS 11.0, *)
  public convenience init<
    T : _ObjectiveCBridgeable
  >(forAccepting _: T.Type) where T._ObjectiveCType : NSItemProviderReading {
    self.init(forAccepting: T._ObjectiveCType.self)
  }

  @available(iOS 11.0, *)
  public func addTypeIdentifiers<
    T : _ObjectiveCBridgeable
  >(forAccepting aClass: T.Type)
  where T._ObjectiveCType : NSItemProviderReading {
    self.addTypeIdentifiers(forAccepting: T._ObjectiveCType.self)
  }
}


extension UIPasteboard {
  @available(iOS 11.0, *)
  public func setObjects<
    T : _ObjectiveCBridgeable
  >(_ objects: [T]) where T._ObjectiveCType : NSItemProviderWriting {
    // Using a simpler `$0 as! T._ObjectiveCType` triggers and assertion in
    // the compiler.
    self.setObjects(objects.map { $0._bridgeToObjectiveC() })
  }

  @available(iOS 11.0, *)
  public func setObjects<
    T : _ObjectiveCBridgeable
  >(
    _ objects: [T],
    localOnly: Bool,
    expirationDate: Date?
  ) where T._ObjectiveCType : NSItemProviderWriting {
    self.setObjects(
      // Using a simpler `$0 as! T._ObjectiveCType` triggers and assertion in
      // the compiler.
      objects.map { $0._bridgeToObjectiveC() },
      localOnly: localOnly,
      expirationDate: expirationDate)
  }
}

#endif

//===----------------------------------------------------------------------===//
// UIPrintError compatibility
//===----------------------------------------------------------------------===//

#if os(iOS)

@available(swift, obsoleted: 4.2, renamed:"UIPrintError.Code.notAvailable.rawValue")
public let UIPrintingNotAvailableError = 1

@available(swift, obsoleted: 4.2, renamed:"UIPrintError.Code.noContent.rawValue")
public let UIPrintNoContentError = 2

@available(swift, obsoleted: 4.2, renamed:"UIPrintError.Code.unknownImageFormat.rawValue")
public let UIPrintUnknownImageFormatError = 3

@available(swift, obsoleted: 4.2, renamed:"UIPrintError.Code.jobFailed.rawValue")
public let UIPrintJobFailedError = 4

#endif
