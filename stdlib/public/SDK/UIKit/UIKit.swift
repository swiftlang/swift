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
@exported import UIKit

// These are un-imported macros in UIKit.

public extension UIDeviceOrientation {
  var isLandscape: Bool { 
    get { return self == .LandscapeLeft  ||  self == .LandscapeRight } 
  }

  var isPortrait: Bool { 
    get { return self == .Portrait  ||  self == .PortraitUpsideDown } 
  }

  var isFlat: Bool {
    get { return self == .FaceUp  ||  self == .FaceDown } 
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

public
func UIDeviceOrientationIsLandscape(orientation: UIDeviceOrientation) -> Bool {
  return orientation.isLandscape
}

public
func UIDeviceOrientationIsPortrait(orientation: UIDeviceOrientation) -> Bool {
  return orientation.isPortrait 
}

public
func UIDeviceOrientationIsValidInterfaceOrientation(
  orientation: UIDeviceOrientation) -> Bool 
{
  return orientation.isValidInterfaceOrientation
}


public extension UIInterfaceOrientation {
  var isLandscape: Bool { 
    get { return self == .LandscapeLeft  ||  self == .LandscapeRight } 
  }

  var isPortrait: Bool { 
    get { return self == .Portrait  ||  self == .PortraitUpsideDown } 
  }
}

public
func UIInterfaceOrientationIsPortrait(orientation: UIInterfaceOrientation) 
  -> Bool 
{
  return orientation.isPortrait
}

public
func UIInterfaceOrientationIsLandscape(orientation: UIInterfaceOrientation) 
  -> Bool 
{
  return orientation.isLandscape
}

// Overlays for variadic initializers.

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

struct _UIViewMirror : MirrorType {
  static var _views = NSMutableSet()

  var _v : UIView
  
  init(_ v : UIView) {_v = v}
  
  var value: Any { get { return _v } }
  
  var valueType: Any.Type { get { return (_v as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 0 } }
  
  subscript(_: Int) -> (String, MirrorType) {
    _preconditionFailure("MirrorType access out of bounds")
  }
  
  var summary: String { get { return ""} }
  
  var quickLookObject: QuickLookObject? {
      // iOS 7 or greater only
      
      var result: QuickLookObject? = nil
      
      switch _UIViewMirror._views.member(_v) {
        case nil:
          _UIViewMirror._views.addObject(_v)
          
          let bounds = _v.bounds
          // in case of an empty rectangle abort the logging
          if (bounds.size.width == 0) || (bounds.size.height == 0) {
            return nil
          }
      
          UIGraphicsBeginImageContext(bounds.size)
      
          var ctx = UIGraphicsGetCurrentContext()
          UIColor(white:1.0, alpha:0.0).set()
          CGContextFillRect(ctx, bounds)
          _v.layer.renderInContext(ctx)
          var maybe_image = UIGraphicsGetImageFromCurrentImageContext()
      
          UIGraphicsEndImageContext()
      
          if let image? = maybe_image {
            result = .Some(.View(image))
          }

          _UIViewMirror._views.removeObject(_v)
          
        default: ()
      }


      return result
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

extension UIView : Reflectable {
  /// Returns a mirror that reflects `self`.
  public func getMirror() -> MirrorType {
    return _UIViewMirror(self)
  }
}

extension UIColor : _ColorLiteralConvertible {
  public required convenience init(colorLiteralRed red: Float, green: Float,
                                   blue: Float, alpha: Float) {
    self.init(red: CGFloat(red), green: CGFloat(green),
              blue: CGFloat(blue), alpha: CGFloat(alpha))
  }
}

public typealias _ColorLiteralType = UIColor

extension UIImage : _ImageLiteralConvertible {
  public required convenience init?(imageLiteral name: String) {
    self.init(named: name)
  }
}

public typealias _ImageLiteralType = UIImage
