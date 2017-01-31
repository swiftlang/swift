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

//===----------------------------------------------------------------------===//
// UIGeometry
//===----------------------------------------------------------------------===//

public extension UIEdgeInsets {
  static var zero: UIEdgeInsets {
    @_transparent // @fragile
    get { return UIEdgeInsets(top: 0.0, left: 0.0, bottom: 0.0, right: 0.0) }
  }
}

public extension UIOffset {
  static var zero: UIOffset {
    @_transparent // @fragile
    get { return UIOffset(horizontal: 0.0, vertical: 0.0) }
  }
}

//===----------------------------------------------------------------------===//
// Equatable types.
//===----------------------------------------------------------------------===//

@_transparent // @fragile
public func == (lhs: UIEdgeInsets, rhs: UIEdgeInsets) -> Bool {
  return lhs.top == rhs.top &&
         lhs.left == rhs.left &&
         lhs.bottom == rhs.bottom &&
         lhs.right == rhs.right
}

extension UIEdgeInsets : Equatable {}

@_transparent // @fragile
public func == (lhs: UIOffset, rhs: UIOffset) -> Bool {
  return lhs.horizontal == rhs.horizontal &&
         lhs.vertical == rhs.vertical
}

extension UIOffset : Equatable {}

// These are un-imported macros in UIKit.

//===----------------------------------------------------------------------===//
// UIDeviceOrientation
//===----------------------------------------------------------------------===//

#if !os(watchOS) && !os(tvOS)
public extension UIDeviceOrientation {
  var isLandscape: Bool {
    return self == .landscapeLeft || self == .landscapeRight
  }

  var isPortrait: Bool {
    return self == .portrait || self == .portraitUpsideDown
  }

  var isFlat: Bool {
    return self == .faceUp || self == .faceDown
  }

  var isValidInterfaceOrientation: Bool {
    switch self {
    case .portrait, .portraitUpsideDown, .landscapeLeft, .landscapeRight:
      return true
    default:
      return false
    }
  }
}

public func UIDeviceOrientationIsLandscape(
  _ orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isLandscape
}

public func UIDeviceOrientationIsPortrait(
  _ orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isPortrait
}

public func UIDeviceOrientationIsValidInterfaceOrientation(
  _ orientation: UIDeviceOrientation) -> Bool
{
  return orientation.isValidInterfaceOrientation
}
#endif

//===----------------------------------------------------------------------===//
// UIInterfaceOrientation
//===----------------------------------------------------------------------===//

#if !os(watchOS) && !os(tvOS)
public extension UIInterfaceOrientation {
  var isLandscape: Bool {
    return self == .landscapeLeft || self == .landscapeRight
  }

  var isPortrait: Bool {
    return self == .portrait || self == .portraitUpsideDown
  }
}

public func UIInterfaceOrientationIsPortrait(
  _ orientation: UIInterfaceOrientation) -> Bool {
  return orientation.isPortrait
}

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
  @nonobjc public required convenience init(colorLiteralRed red: Float,
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
