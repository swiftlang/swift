//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
@_exported import UIKit


//===----------------------------------------------------------------------===//
// Equatable types.
//===----------------------------------------------------------------------===//

@_transparent // @fragile
@warn_unused_result
public func == (lhs: UIEdgeInsets, rhs: UIEdgeInsets) -> Bool {
  return lhs.top == rhs.top &&
         lhs.left == rhs.left &&
         lhs.bottom == rhs.bottom &&
         lhs.right == rhs.right
}

extension UIEdgeInsets : Equatable {}

@_transparent // @fragile
@warn_unused_result
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
    get { return self == .LandscapeLeft || self == .LandscapeRight }
  }

  var isPortrait: Bool {
    get { return self == .Portrait || self == .PortraitUpsideDown }
  }

  var isFlat: Bool {
    get { return self == .FaceUp || self == .FaceDown }
  }

  var isValidInterfaceOrientation: Bool {
    get {
      switch (self) {
        case .Portrait, .PortraitUpsideDown, .LandscapeLeft, .LandscapeRight:
          return true
        default:
          return false
      }
    }
  }
}

@warn_unused_result
public func UIDeviceOrientationIsLandscape(
  orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isLandscape
}

@warn_unused_result
public func UIDeviceOrientationIsPortrait(
  orientation: UIDeviceOrientation
) -> Bool {
  return orientation.isPortrait
}

@warn_unused_result
public func UIDeviceOrientationIsValidInterfaceOrientation(
  orientation: UIDeviceOrientation) -> Bool
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
    get { return self == .LandscapeLeft || self == .LandscapeRight }
  }

  var isPortrait: Bool {
    get { return self == .Portrait || self == .PortraitUpsideDown }
  }
}

@warn_unused_result
public func UIInterfaceOrientationIsPortrait(
  orientation: UIInterfaceOrientation) -> Bool {
  return orientation.isPortrait
}

@warn_unused_result
public func UIInterfaceOrientationIsLandscape(
  orientation: UIInterfaceOrientation
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
    self.addButtonWithTitle(firstButtonTitle)
    for buttonTitle in moreButtonTitles {
      self.addButtonWithTitle(buttonTitle)
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
    self.addButtonWithTitle(firstButtonTitle)
    for buttonTitle in moreButtonTitles {
      self.addButtonWithTitle(buttonTitle)
    }
  }
}
#endif

#if !os(watchOS)
struct _UIViewMirror : _Mirror {
  static var _views = NSMutableSet()

  var _v : UIView
  
  init(_ v : UIView) { _v = v }
  
  var value: Any { get { return _v } }
  
  var valueType: Any.Type { get { return (_v as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 0 } }
  
  subscript(_: Int) -> (String, _Mirror) {
    _requirementFailure("_Mirror access out of bounds")
  }
  
  var summary: String { get { return "" } }
  
  var quickLookObject: PlaygroundQuickLook? {
      // iOS 7 or greater only
      
      var result: PlaygroundQuickLook? = nil
      
      switch _UIViewMirror._views.member(_v) {
        case nil:
          _UIViewMirror._views.addObject(_v)
          
          let bounds = _v.bounds
          // in case of an empty rectangle abort the logging
          if (bounds.size.width == 0) || (bounds.size.height == 0) {
            return nil
          }
      
          UIGraphicsBeginImageContextWithOptions(bounds.size, false, 0.0)

          // UIKit is about to update this to be optional, so make it work
          // with both older and newer SDKs. (In this context it should always
          // be present.)
          let ctx: CGContext! = UIGraphicsGetCurrentContext()
          UIColor(white:1.0, alpha:0.0).set()
          CGContextFillRect(ctx, bounds)
          _v.layer.renderInContext(ctx)

          let image: UIImage! = UIGraphicsGetImageFromCurrentImageContext()
      
          UIGraphicsEndImageContext()
      
          result = .Some(.View(image))

          _UIViewMirror._views.removeObject(_v)
          
        default: ()
      }


      return result
  }
  
  var disposition : _MirrorDisposition { get { return .Aggregate } }
}

extension UIView : _Reflectable {
  /// Returns a mirror that reflects `self`.
  public func _getMirror() -> _Mirror {
    return _UIViewMirror(self)
  }
}
#endif

extension UIColor : _ColorLiteralConvertible {
  public required convenience init(colorLiteralRed red: Float, green: Float,
                                   blue: Float, alpha: Float) {
    self.init(red: CGFloat(red), green: CGFloat(green),
              blue: CGFloat(blue), alpha: CGFloat(alpha))
  }
}

public typealias _ColorLiteralType = UIColor

extension UIImage : _ImageLiteralConvertible {
  private convenience init!(failableImageLiteral name: String) {
    self.init(named: name)
  }

  public required convenience init(imageLiteral name: String) {
    self.init(failableImageLiteral: name)
  }
}

public typealias _ImageLiteralType = UIImage
