//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
@_exported import AppKit

struct _NSCursorMirror : _MirrorType {
  var _value: NSCursor

  init(_ v: NSCursor) { _value = v }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 0 }

  subscript(_: Int) -> (String, _MirrorType) {
    _preconditionFailure("_MirrorType access out of bounds")
  }

  var summary: String { return "" }

  var quickLookObject: PlaygroundQuickLook? {
    return .Some(.Image(_value.image))
  }
  
  var disposition : _MirrorDisposition { return .Aggregate }
}

extension NSCursor : _Reflectable {
  public func _getMirror() -> _MirrorType {
    return _NSCursorMirror(self)
  }
}

struct _NSViewMirror : _MirrorType {
  static var _views = Set<NSView>()

  var _v : NSView
  
  init(_ v : NSView) { _v = v }
  
  var value: Any { return _v }
  
  var valueType: Any.Type { return (_v as Any).dynamicType }
  
  var objectIdentifier: ObjectIdentifier? { return .None }
  
  var count: Int { return 0 }
  
  subscript(_: Int) -> (String, _MirrorType) {
    _preconditionFailure("_MirrorType access out of bounds")
  }
  
  var summary: String { return "" }
  
  var quickLookObject: PlaygroundQuickLook? {
    // adapted from the Xcode QuickLooks implementation
    
    var result: PlaygroundQuickLook? = nil
    
    // if you set NSView.needsDisplay, you can get yourself in a recursive scenario where the same view
    // could need to draw itself in order to get a QLObject for itself, which in turn if your code was
    // instrumented to log on-draw, would cause yourself to get back here and so on and so forth
    // until you run out of stack and crash
    // This code checks that we aren't trying to log the same view recursively - and if so just returns
    // nil, which is probably a safer option than crashing
    // FIXME: is there a way to say "cacheDisplayInRect butDoNotRedrawEvenIfISaidSo"?
    if !_NSViewMirror._views.contains(_v) {
      _NSViewMirror._views.insert(_v)
      
      let bounds = _v.bounds
      if let b = _v.bitmapImageRepForCachingDisplayInRect(bounds) {
        _v.cacheDisplayInRect(bounds, toBitmapImageRep: b)
        result = .Some(.View(b))
      }
      
      _NSViewMirror._views.remove(_v)
    }
    
    return result
    
  }
  
  var disposition : _MirrorDisposition { return .Aggregate }
}

extension NSView : _Reflectable {
  /// Returns a mirror that reflects `self`.
  public func _getMirror() -> _MirrorType {
    return _NSViewMirror(self)
  }
}

// Overlays for variadics.

public extension NSGradient {
  convenience init?(colorsAndLocations objects: (NSColor, CGFloat)...) {
    self.init(
      colors: objects.map { $0.0 },
      atLocations: objects.map { $0.1 },
      colorSpace: NSColorSpace.genericRGBColorSpace())
  }
}

// Fix the ARGV type of NSApplicationMain, which nonsensically takes
// argv as a const char**.
@_silgen_name("NSApplicationMain")
public func NSApplicationMain(
  argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>>
) -> Int32

extension NSColor : _ColorLiteralConvertible {
  public required convenience init(colorLiteralRed red: Float, green: Float,
                                   blue: Float, alpha: Float) {
    self.init(SRGBRed: CGFloat(red), green: CGFloat(green),
              blue: CGFloat(blue), alpha: CGFloat(alpha))
  }
}

public typealias _ColorLiteralType = NSColor

extension NSImage : _ImageLiteralConvertible {
  private convenience init!(failableImageLiteral name: String) {
    self.init(named: name)
  }

  public required convenience init(imageLiteral name: String) {
    self.init(failableImageLiteral: name)
  }
}

public typealias _ImageLiteralType = NSImage
