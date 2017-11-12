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

extension NSRect {
  /// Fills this rect in the current NSGraphicsContext in the context's fill
  /// color.
  /// The compositing operation of the fill defaults to the context's
  /// compositing operation, not necessarily using `.copy` like `NSRectFill()`.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func fill(using operation: NSCompositingOperation =
    NSGraphicsContext.current?.compositingOperation ?? .sourceOver) {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    __NSRectFillUsingOperation(self, operation)
  }
    
  /// Draws a frame around the inside of this rect in the current
  /// NSGraphicsContext in the context's fill color
  /// The compositing operation of the fill defaults to the context's
  /// compositing operation, not necessarily using `.copy` like `NSFrameRect()`.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func frame(withWidth width: CGFloat = 1.0,
                    using operation: NSCompositingOperation =
    NSGraphicsContext.current?.compositingOperation ?? .sourceOver) {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    __NSFrameRectWithWidthUsingOperation(self, width, operation)
  }
    
  /// Modifies the current graphics context clipping path by intersecting it
  /// with this rect.
  /// This permanently modifies the graphics state, so the current state should
  /// be saved beforehand and restored afterwards.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func clip() {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    __NSRectClip(self)
  }
}

extension Sequence where Iterator.Element == NSRect {
  /// Fills this list of rects in the current NSGraphicsContext in the context's
  /// fill color.
  /// The compositing operation of the fill defaults to the context's
  /// compositing operation, not necessarily using `.copy` like `NSRectFill()`.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func fill(using operation: NSCompositingOperation =
    NSGraphicsContext.current?.compositingOperation ?? .sourceOver) {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    let rects = Array(self)
    let count = rects.count
    guard count > 0 else { return }
    rects.withUnsafeBufferPointer { rectBufferPointer in
      guard let rectArray = rectBufferPointer.baseAddress else { return }
      __NSRectFillListUsingOperation(rectArray, count, operation)
    }
  }
    
  /// Modifies the current graphics context clipping path by intersecting it
  /// with the graphical union of this list of rects
  /// This permanently modifies the graphics state, so the current state should
  /// be saved beforehand and restored afterwards.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func clip() {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    let rects = Array(self)
    let count = rects.count
    guard count > 0 else { return }
    rects.withUnsafeBufferPointer { rectBufferPointer in
      guard let rectArray = rectBufferPointer.baseAddress else { return }
      __NSRectClipList(rectArray, count)
    }
  }
}

extension Sequence where Iterator.Element == (CGRect, NSColor) {
  /// Fills this list of rects in the current NSGraphicsContext with that rect's
  /// associated color
  /// The compositing operation of the fill defaults to the context's
  /// compositing operation, not necessarily using `.copy` like `NSRectFill()`.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func fill(using operation: NSCompositingOperation =
    NSGraphicsContext.current?.compositingOperation ?? .sourceOver) {
    precondition(NSGraphicsContext.current != nil,
                 "There must be a set current NSGraphicsContext")
    let rects = map { $0.0 }
    let colors = map { $0.1 }
    let count = rects.count
    guard count > 0 else { return }
    rects.withUnsafeBufferPointer { rectBufferPointer in
      colors.withUnsafeBufferPointer { colorBufferPointer in
        guard let rectArray = rectBufferPointer.baseAddress else { return }
        guard let colorArray = colorBufferPointer.baseAddress else { return }
        __NSRectFillListWithColorsUsingOperation(
            rectArray, colorArray, count, operation)
      }
    }
  }
}

extension Sequence where Iterator.Element == (CGRect, gray: CGFloat) {
  /// Fills this list of rects in the current NSGraphicsContext with that rect's
  /// associated gray component value in the DeviceGray color space.
  /// The compositing operation of the fill defaults to the context's
  /// compositing operation, not necessarily using `.copy` like
  /// `NSRectFillListWithGrays()`.
  /// - precondition: There must be a set current NSGraphicsContext.
  @available(swift 4)
  public func fill(using operation: NSCompositingOperation =
    NSGraphicsContext.current?.compositingOperation ?? .sourceOver) {
    // NSRectFillListWithGrays does not have a variant taking an operation, but
    // is added here for consistency with the other drawing operations.
    guard let graphicsContext = NSGraphicsContext.current else {
      fatalError("There must be a set current NSGraphicsContext")
    }
    let cgContext: CGContext
    if #available(macOS 10.10, *) {
      cgContext = graphicsContext.cgContext
    } else {
      cgContext = Unmanaged<CGContext>.fromOpaque(
        graphicsContext.graphicsPort).takeUnretainedValue()
    }
    cgContext.saveGState()
    forEach {
      cgContext.setFillColor(gray: $0.gray, alpha: 1.0)
      __NSRectFillUsingOperation($0.0, operation)
    }
    cgContext.restoreGState()
  }
}

extension NSWindow.Depth {
  @available(swift 4)
  public static func bestDepth(
    colorSpaceName: NSColorSpaceName,
    bitsPerSample: Int,
    bitsPerPixel: Int,
    isPlanar: Bool
    ) -> (NSWindow.Depth, isExactMatch: Bool) {
    var isExactMatch: ObjCBool = false
    let depth = __NSBestDepth(
        colorSpaceName,
        bitsPerSample, bitsPerPixel, isPlanar, &isExactMatch)
    return (depth, isExactMatch: isExactMatch.boolValue)
  }
  @available(swift 4)
  public static var availableDepths: [NSWindow.Depth] {
    // __NSAvailableWindowDepths is NULL terminated, the length is not known up front
    let depthsCArray = __NSAvailableWindowDepths()
    var depths: [NSWindow.Depth] = []
    var length = 0
    var depth = depthsCArray[length]
    while depth.rawValue != 0 {
      depths.append(depth)
      length += 1
      depth = depthsCArray[length]
    }
    return depths
  }
}

extension NSAnimationEffect {
  private class _CompletionHandlerDelegate : NSObject {
    var completionHandler: () -> Void = { }
    @objc func animationEffectDidEnd(_ contextInfo: UnsafeMutableRawPointer?) {
      completionHandler()
    }
  }
  @available(swift 4)
  public func show(centeredAt centerLocation: NSPoint, size: NSSize,
                   completionHandler: @escaping () -> Void = { }) {
    let delegate = _CompletionHandlerDelegate()
    delegate.completionHandler = completionHandler
    // Note that the delegate of `__NSShowAnimationEffect` is retained for the
    // duration of the animation.
    __NSShowAnimationEffect(
        self,
        centerLocation,
        size,
        delegate,
        #selector(_CompletionHandlerDelegate.animationEffectDidEnd(_:)),
        nil)
  }
}

extension NSSound {
  @available(swift 4)
  public static func beep() {
    __NSBeep()
  }
}
