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
@_exported import AppKit

extension NSCursor : _DefaultCustomPlaygroundQuickLookable {
  public var _defaultCustomPlaygroundQuickLook: PlaygroundQuickLook {
    return .image(image)
  }
}

internal struct _NSViewQuickLookState {
  static var views = Set<NSView>()
}

extension NSView : _DefaultCustomPlaygroundQuickLookable {
  public var _defaultCustomPlaygroundQuickLook: PlaygroundQuickLook {
    // if you set NSView.needsDisplay, you can get yourself in a recursive scenario where the same view
    // could need to draw itself in order to get a QLObject for itself, which in turn if your code was
    // instrumented to log on-draw, would cause yourself to get back here and so on and so forth
    // until you run out of stack and crash
    // This code checks that we aren't trying to log the same view recursively - and if so just returns
    // an empty view, which is probably a safer option than crashing
    // FIXME: is there a way to say "cacheDisplayInRect butDoNotRedrawEvenIfISaidSo"?
    if _NSViewQuickLookState.views.contains(self) {
      return .view(NSImage())
    } else {
      _NSViewQuickLookState.views.insert(self)
      let result: PlaygroundQuickLook
      if let b = bitmapImageRepForCachingDisplay(in: bounds) {
        cacheDisplay(in: bounds, to: b)
        result = .view(b)
      } else {
        result = .view(NSImage())
      }
      _NSViewQuickLookState.views.remove(self)
      return result
    }
  }
}

// Overlays for variadics.

public extension NSGradient {
  convenience init?(colorsAndLocations objects: (NSColor, CGFloat)...) {
    self.init(
      colors: objects.map { $0.0 },
      atLocations: objects.map { $0.1 },
      colorSpace: NSColorSpace.genericRGB)
  }
}

// Fix the ARGV type of NSApplicationMain, which nonsensically takes
// argv as a const char**.
public func NSApplicationMain(
  _ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>
) -> Int32 {
  return argv.withMemoryRebound(to: UnsafePointer<CChar>.self, capacity: Int(argc)) {
    __NSApplicationMain(argc, $0)
  }
}

extension NSApplication {
  @available(swift 4)
  public static func loadApplication() {
    __NSApplicationLoad()
  }
}

extension NSColor : _ExpressibleByColorLiteral {
  @nonobjc
  public required convenience init(_colorLiteralRed red: Float, green: Float,
                                   blue: Float, alpha: Float) {
    self.init(red: CGFloat(red), green: CGFloat(green),
              blue: CGFloat(blue), alpha: CGFloat(alpha))
  }
}

public typealias _ColorLiteralType = NSColor

extension NSImage : _ExpressibleByImageLiteral {
  private convenience init!(failableImageLiteral name: String) {
    self.init(named: .init(name))
  }

  @nonobjc
  public required convenience init(imageLiteralResourceName name: String) {
    self.init(failableImageLiteral: name)
  }
}

public typealias _ImageLiteralType = NSImage

// Extensions for common numeric enum operations

@available(swift 4)
public protocol _AppKitNumericWrapper : RawRepresentable { }
extension _AppKitNumericWrapper where RawValue == Float {
  public static func +(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue + Float(rhs))!
  }
  public static func -(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue - Float(rhs))!
  }
  public static func +(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func -(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func +(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue + Float(rhs))!
  }
  public static func -(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue - Float(rhs))!
  }
  public static func +(lhs: Float, rhs: Self) -> Self {
    return Self(rawValue: lhs + rhs.rawValue)!
  }
  public static func +(lhs: Int, rhs: Self) -> Self {
    return Self(rawValue: Float(lhs) + rhs.rawValue)!
  }
  public static func +(lhs: Double, rhs: Self) -> Self {
    return Self(rawValue: Float(lhs) + rhs.rawValue)!
  }
  public static func += (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue + Float(rhs))!
  }
  public static func += (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func += (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue + Float(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue - Float(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func -= (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue - Float(rhs))!
  }
}
extension _AppKitNumericWrapper where RawValue == CGFloat {
  public static func +(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func -(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
  public static func +(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func -(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
  public static func +(lhs: Self, rhs: CGFloat) -> Self {
    return Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func -(lhs: Self, rhs: CGFloat) -> Self {
    return Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func +(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func -(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
  public static func +(lhs: Float, rhs: Self) -> Self {
    return Self(rawValue: CGFloat(lhs) + rhs.rawValue)!
  }
  public static func +(lhs: Int, rhs: Self) -> Self {
    return Self(rawValue: CGFloat(lhs) + rhs.rawValue)!
  }
  public static func +(lhs: Double, rhs: Self) -> Self {
    return Self(rawValue: CGFloat(lhs) + rhs.rawValue)!
  }
  public static func += (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func += (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func += (lhs: inout Self, rhs: CGFloat) {
      lhs = Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func += (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue + CGFloat(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: CGFloat) {
      lhs = Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func -= (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue - CGFloat(rhs))!
  }
}
extension _AppKitNumericWrapper where RawValue == Double {
  public static func +(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func -(lhs: Self, rhs: Double) -> Self {
    return Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func +(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue + Double(rhs))!
  }
  public static func -(lhs: Self, rhs: Float) -> Self {
    return Self(rawValue: lhs.rawValue - Double(rhs))!
  }
  public static func +(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue + Double(rhs))!
  }
  public static func -(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue - Double(rhs))!
  }
  public static func +(lhs: Float, rhs: Self) -> Self {
    return Self(rawValue: Double(lhs) + rhs.rawValue)!
  }
  public static func +(lhs: Int, rhs: Self) -> Self {
    return Self(rawValue: Double(lhs) + rhs.rawValue)!
  }
  public static func +(lhs: Double, rhs: Self) -> Self {
    return Self(rawValue: lhs + rhs.rawValue)!
  }
  public static func += (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func += (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue + Double(rhs))!
  }
  public static func += (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue + Double(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Double) {
      lhs = Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func -= (lhs: inout Self, rhs: Float) {
      lhs = Self(rawValue: lhs.rawValue - Double(rhs))!
  }
  public static func -= (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue - Double(rhs))!
  }
}
extension _AppKitNumericWrapper where RawValue == Int {
  public static func +(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func -(lhs: Self, rhs: Int) -> Self {
    return Self(rawValue: lhs.rawValue - rhs)!
  }
  public static func +(lhs: Int, rhs: Self) -> Self {
    return Self(rawValue: lhs + rhs.rawValue)!
  }
  public static func += (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue + rhs)!
  }
  public static func -= (lhs: inout Self, rhs: Int) {
      lhs = Self(rawValue: lhs.rawValue - rhs)!
  }
}

@available(swift 4)
extension NSAppKitVersion : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSAppKitVersion, rhs: NSAppKitVersion) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSAppKitVersion, rhs: NSAppKitVersion) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4)
extension NSLayoutConstraint.Priority : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSLayoutConstraint.Priority, rhs: NSLayoutConstraint.Priority) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSLayoutConstraint.Priority, rhs: NSLayoutConstraint.Priority) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4)
extension NSStackView.VisibilityPriority : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSStackView.VisibilityPriority, rhs: NSStackView.VisibilityPriority) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSStackView.VisibilityPriority, rhs: NSStackView.VisibilityPriority) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4)
extension NSToolbarItem.VisibilityPriority : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSToolbarItem.VisibilityPriority, rhs: NSToolbarItem.VisibilityPriority) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSToolbarItem.VisibilityPriority, rhs: NSToolbarItem.VisibilityPriority) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4) @available(macOS 10.12.2, *)
extension NSTouchBarItem.Priority : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSTouchBarItem.Priority, rhs: NSTouchBarItem.Priority) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSTouchBarItem.Priority, rhs: NSTouchBarItem.Priority) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4)
extension NSWindow.Level : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSWindow.Level, rhs: NSWindow.Level) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSWindow.Level, rhs: NSWindow.Level) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
@available(swift 4)
extension NSFont.Weight : _AppKitNumericWrapper, Comparable {
  public static func <(lhs: NSFont.Weight, rhs: NSFont.Weight) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func >(lhs: NSFont.Weight, rhs: NSFont.Weight) -> Bool {
    return lhs.rawValue > rhs.rawValue
  }
}
