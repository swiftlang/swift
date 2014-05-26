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

// Overlay UIApplicationMain, which is declared with a non-const char *argv[]
// argument that gets mapped to UnsafePointer<UnsafePointer<CChar>> by the
// importer, with one that takes UnsafePointer<CString> instead, matching the
// type of C_ARGV.
@asmname("UIApplicationMain")
func UIApplicationMain(argc: CInt,
                       argv: UnsafePointer<CString>,
                       principalClassName: NSString?,
                       delegateClassName: NSString?) -> CInt

// These are un-imported macros in UIKit.

extension UIDeviceOrientation {
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

func UIDeviceOrientationIsLandscape(orientation: UIDeviceOrientation) -> Bool {
  return orientation.isLandscape
}

func UIDeviceOrientationIsPortrait(orientation: UIDeviceOrientation) -> Bool {
  return orientation.isPortrait 
}

func UIDeviceOrientationIsValidInterfaceOrientation(
  orientation: UIDeviceOrientation) -> Bool 
{
  return orientation.isValidInterfaceOrientation
}


extension UIInterfaceOrientation {
  var isLandscape: Bool { 
    get { return self == .LandscapeLeft  ||  self == .LandscapeRight } 
  }

  var isPortrait: Bool { 
    get { return self == .Portrait  ||  self == .PortraitUpsideDown } 
  }
}

func UIInterfaceOrientationIsPortrait(orientation: UIInterfaceOrientation) 
  -> Bool 
{
  return orientation.isPortrait
}

func UIInterfaceOrientationIsLandscape(orientation: UIInterfaceOrientation) 
  -> Bool 
{
  return orientation.isLandscape
}

// Overlays for variadic initializers.

extension UIActionSheet {
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

extension UIAlertView {
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

struct _UIViewMirror : Mirror {
  var _v : UIView
  
  init(_ v : UIView) {_v = v}
  
  var value: Any { get { return _v } }
  
  var valueType: Any.Type { get { return (_v as Any).dynamicType } }
  
  var objectIdentifier: ObjectIdentifier? { get { return .None } }
  
  var count: Int { get { return 0 } }
  
  subscript(_: Int) -> (String,Mirror) { get { _fatalError("don't ask") } }
  
  var summary: String { get { return ""} }
  
  var quickLookObject: QuickLookObject? {
      // iOS 7 or greater only
      
      let bounds = _v.bounds
      
      // in case of an empty rectangle abort the logging
      if (bounds.size.width == 0) || (bounds.size.height == 0) {
        return nil
      }
      
      UIGraphicsBeginImageContext(bounds.size)
      
      var ctx = UIGraphicsGetCurrentContext().takeUnretainedValue()
      
      UIColor(white:1.0, alpha:0.0).set()
      
      CGContextFillRect(ctx, bounds)
      
      _v.layer.renderInContext(ctx)
      
      var maybe_image = UIGraphicsGetImageFromCurrentImageContext()
      
      UIGraphicsEndImageContext()
      
      if let image = maybe_image {
        return .Some(.View(image))
      }
      return nil
  }
  
  var disposition : MirrorDisposition { get { return .Aggregate } }
}

extension UIView : Reflectable {
  func getMirror() -> Mirror {
    return _UIViewMirror(self)
  }
}
