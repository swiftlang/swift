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

@_exported import CoreGraphics
import Darwin

//===----------------------------------------------------------------------===//
// CGAffineTransform
//===----------------------------------------------------------------------===//

extension CGAffineTransform: Equatable {
  public static func ==(lhs: CGAffineTransform,
                        rhs: CGAffineTransform) -> Bool {
    return lhs.__equalTo(rhs)
  }
}

//===----------------------------------------------------------------------===//
// CGColor
//===----------------------------------------------------------------------===//

extension CGColor {
  @available(macOS 10.3, iOS 2.0, *)
  public var components: [CGFloat]? {
    guard let pointer = self.__unsafeComponents else { return nil }
    let buffer = UnsafeBufferPointer(start: pointer, count: self.numberOfComponents)
    return Array(buffer)
  }

#if os(macOS)
  public class var white: CGColor
   { return CGColor.__constantColor(for: CGColor.__whiteColorName)! }

  public class var black: CGColor
   { return CGColor.__constantColor(for: CGColor.__blackColorName)! }

  public class var clear: CGColor
   { return CGColor.__constantColor(for: CGColor.__clearColorName)! }
#endif
}

public protocol _CGColorInitTrampoline {
  init(red: CGFloat, green: CGFloat, blue: CGFloat, alpha: CGFloat)
}

extension _CGColorInitTrampoline {
  public init(_colorLiteralRed red: Float, green: Float, blue: Float,
              alpha: Float) {
    self.init(red: CGFloat(red), green: CGFloat(green), blue: CGFloat(blue),
              alpha: CGFloat(alpha))
  }
}

extension CGColor : _CGColorInitTrampoline, _ExpressibleByColorLiteral { }

//===----------------------------------------------------------------------===//
// CGColorSpace
//===----------------------------------------------------------------------===//

extension CGColorSpace {
  public var colorTable: [UInt8]? {
    guard self.model == .indexed else { return nil }
    let components = self.baseColorSpace?.numberOfComponents ?? 1
    var table = [UInt8](repeating: 0, count: self.__colorTableCount * components)
    self.__unsafeGetColorTable(&table)
    return table
  }
}

//===----------------------------------------------------------------------===//
// CGContext
//===----------------------------------------------------------------------===//

extension CGContext {

  public func setLineDash(phase: CGFloat, lengths: [CGFloat]) {
    self.__setLineDash(phase: phase, lengths: lengths, count: lengths.count)
  }

  public func move(to point: CGPoint) {
    self.__moveTo(x: point.x, y: point.y)
  }

  public func addLine(to point: CGPoint) {
    self.__addLineTo(x: point.x, y: point.y)
  }

  public func addCurve(to end: CGPoint, control1: CGPoint, control2: CGPoint) {
    self.__addCurveTo(cp1x: control1.x, cp1y: control1.y,
     cp2x: control2.x, cp2y: control2.y, endingAtX: end.x, y: end.y)
  }

  public func addQuadCurve(to end: CGPoint, control: CGPoint) {
    self.__addQuadCurveTo(cpx: control.x, cpy: control.y,
     endingAtX: end.x, y: end.y)
  }

  public func addRects(_ rects: [CGRect]) {
    self.__addRects(rects, count: rects.count)
  }

  public func addLines(between points: [CGPoint]) {
    self.__addLines(between: points, count: points.count)
  }

  public func addArc(center: CGPoint, radius: CGFloat, startAngle: CGFloat,
   endAngle: CGFloat, clockwise: Bool) {
    self.__addArc(centerX: center.x, y: center.y, radius: radius,
     startAngle: startAngle, endAngle: endAngle, clockwise: clockwise ? 1 : 0)
  }

  public func addArc(tangent1End: CGPoint, tangent2End: CGPoint,
   radius: CGFloat) {
    self.__addArc(x1: tangent1End.x, y1: tangent1End.y, 
    x2: tangent2End.x, y2: tangent2End.y, radius: radius)
  }

  /// Fills the current path using the specified rule (winding by default).
  ///
  /// Any open subpath is implicitly closed.
  public func fillPath(using rule: CGPathFillRule = .winding) {
    switch rule {
      case .winding: self.__fillPath()
      case .evenOdd: self.__eoFillPath()
    }
  }
  
  /// Intersects the current path with the current clipping region and uses the
  /// result as the new clipping region for subsequent drawing.
  ///
  /// Uses the specified fill rule (winding by default) to determine which
  /// areas to treat as the interior of the clipping region. When evaluating
  /// the path, any open subpath is implicitly closed.
  public func clip(using rule: CGPathFillRule = .winding) {
    switch rule {
      case .winding: self.__clip()
      case .evenOdd: self.__eoClip()
    }
  }

  public func fill(_ rects: [CGRect]) {
    self.__fill(rects, count: rects.count)
  }

  public func strokeLineSegments(between points: [CGPoint]) {
    self.__strokeLineSegments(between: points, count: points.count)
  }

  public func clip(to rects: [CGRect]) {
    self.__clip(to: rects, count: rects.count)
  }

  public func draw(_ image: CGImage, in rect: CGRect, byTiling: Bool = false) {
    if byTiling {
      self.__draw(in: rect, byTiling: image)
    } else {
      self.__draw(in: rect, image: image)
    }
  }

  public var textPosition: CGPoint {
    get { return self.__textPosition }
    set { self.__setTextPosition(x: newValue.x, y: newValue.y) }
  }

  public func showGlyphs(_ glyphs: [CGGlyph], at positions: [CGPoint]) {
    precondition(glyphs.count == positions.count)
    self.__showGlyphs(glyphs, atPositions: positions, count: glyphs.count)
  }

}

//===----------------------------------------------------------------------===//
// CGDataProvider
//===----------------------------------------------------------------------===//

// TODO: replace init(UnsafePointer<UInt8>) with init(String)
// blocked on rdar://problem/27444567

//===----------------------------------------------------------------------===//
// CGDirectDisplay
//===----------------------------------------------------------------------===//

#if os(macOS)
public func CGGetLastMouseDelta() -> (x: Int32, y: Int32) {
  var pair: (x: Int32, y: Int32) = (0, 0)
  __CGGetLastMouseDelta(&pair.x, &pair.y)
  return pair
}
#endif

//===----------------------------------------------------------------------===//
// CGGeometry
//===----------------------------------------------------------------------===//

public extension CGPoint {
  static var zero: CGPoint {
    @_transparent // @fragile
    get { return CGPoint(x: 0, y: 0) }
  }

  @_transparent // @fragile
  init(x: Int, y: Int) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }

  @_transparent // @fragile
  init(x: Double, y: Double) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }
  
  init?(dictionaryRepresentation dict: CFDictionary) {
    var point = CGPoint()
    if CGPoint.__setFromDictionaryRepresentation(dict, &point) {
      self = point
    } else {
      return nil
    }
  }
}

extension CGPoint : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["x": x, "y": y], displayStyle: .`struct`)
  }
}

extension CGPoint : _CustomPlaygroundQuickLookable {
  @available(*, deprecated, message: "CGPoint.customPlaygroundQuickLook will be removed in a future Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .point(Double(x), Double(y))
  }
}

extension CGPoint : CustomDebugStringConvertible {
  public var debugDescription: String {
    return "(\(x), \(y))"
  }
}

extension CGPoint : Equatable {
  @_transparent // @fragile
  public static func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
    return lhs.x == rhs.x  &&  lhs.y == rhs.y
  }
}

extension CGPoint : Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let x = try container.decode(CGFloat.self)
    let y = try container.decode(CGFloat.self)
    self.init(x: x, y: y)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(x)
    try container.encode(y)
  }
}

public extension CGSize {
  static var zero: CGSize {
    @_transparent // @fragile
    get { return CGSize(width: 0, height: 0) }
  }

  @_transparent // @fragile
  init(width: Int, height: Int) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }

  @_transparent // @fragile
  init(width: Double, height: Double) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }

  init?(dictionaryRepresentation dict: CFDictionary) {
    var size = CGSize()
    if CGSize.__setFromDictionaryRepresentation(dict, &size) {
      self = size
    } else {
      return nil
    }
  }
}

extension CGSize : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["width": width, "height": height],
      displayStyle: .`struct`)
  }
}

extension CGSize : _CustomPlaygroundQuickLookable {
  @available(*, deprecated, message: "CGSize.customPlaygroundQuickLook will be removed in a future Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .size(Double(width), Double(height))
  }
}

extension CGSize : CustomDebugStringConvertible {
  public var debugDescription : String {
    return "(\(width), \(height))"
  }
}

extension CGSize : Equatable {
  @_transparent // @fragile
  public static func == (lhs: CGSize, rhs: CGSize) -> Bool {
    return lhs.width == rhs.width  &&  lhs.height == rhs.height
  }
}

extension CGSize : Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let width = try container.decode(CGFloat.self)
    let height = try container.decode(CGFloat.self)
    self.init(width: width, height: height)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(width)
    try container.encode(height)
  }
}

public extension CGVector {
  static var zero: CGVector {
    @_transparent // @fragile
    get { return CGVector(dx: 0, dy: 0) }
  }

  @_transparent // @fragile
  init(dx: Int, dy: Int) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }

  @_transparent // @fragile
  init(dx: Double, dy: Double) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }
}

extension CGVector : Equatable {
  @_transparent // @fragile
  public static func == (lhs: CGVector, rhs: CGVector) -> Bool {
    return lhs.dx == rhs.dx  &&  lhs.dy == rhs.dy
  }
}

extension CGVector : CustomDebugStringConvertible {
  public var debugDescription : String {
    return "(\(dx), \(dy))"
  }
}

extension CGVector : Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let dx = try container.decode(CGFloat.self)
    let dy = try container.decode(CGFloat.self)
    self.init(dx: dx, dy: dy)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(dx)
    try container.encode(dy)
  }
}

public extension CGRect {
  static var zero: CGRect {
    @_transparent // @fragile
    get { return CGRect(x: 0, y: 0, width: 0, height: 0) }
  }

  @_transparent // @fragile
  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {
    self.init(origin: CGPoint(x: x, y: y),
              size: CGSize(width: width, height: height))
  }

  @_transparent // @fragile
  init(x: Double, y: Double, width: Double, height: Double) {
    self.init(origin: CGPoint(x: x, y: y),
              size: CGSize(width: width, height: height))
  }

  @_transparent // @fragile
  init(x: Int, y: Int, width: Int, height: Int) {
    self.init(origin: CGPoint(x: x, y: y),
              size: CGSize(width: width, height: height))
  }

  init?(dictionaryRepresentation dict: CFDictionary) {
    var rect = CGRect()
    if CGRect.__setFromDictionaryRepresentation(dict, &rect) {
      self = rect
    } else {
      return nil
    }
  }

  @_transparent // @fragile
  func divided(atDistance: CGFloat, from fromEdge: CGRectEdge)
    -> (slice: CGRect, remainder: CGRect)
  {
    var slice = CGRect.zero
    var remainder = CGRect.zero
    self.__divided(slice: &slice, remainder: &remainder, atDistance: atDistance,
           from: fromEdge)
    return (slice, remainder)
  }

  @available(*, unavailable, renamed: "minX")
  var x: CGFloat { return minX }

  @available(*, unavailable, renamed: "minY")
  var y: CGFloat { return minY }
}

extension CGRect : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["origin": origin, "size": size],
      displayStyle: .`struct`)
  }
}

extension CGRect : _CustomPlaygroundQuickLookable {
  @available(*, deprecated, message: "CGRect.customPlaygroundQuickLook will be removed in a future Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .rectangle(
      Double(origin.x), Double(origin.y),
      Double(size.width), Double(size.height))
  }
}

extension CGRect : CustomDebugStringConvertible {
  public var debugDescription : String {
    return "(\(origin.x), \(origin.y), \(size.width), \(size.height))"
  }
}

extension CGRect : Equatable {
  @_transparent // @fragile
  public static func == (lhs: CGRect, rhs: CGRect) -> Bool {
    return lhs.equalTo(rhs)
  }
}

extension CGRect : Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let origin = try container.decode(CGPoint.self)
    let size = try container.decode(CGSize.self)
    self.init(origin: origin, size: size)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(origin)
    try container.encode(size)
  }
}

extension CGAffineTransform {
  public static var identity: CGAffineTransform {
   @_transparent // @fragile
   get { return CGAffineTransform(a: 1, b: 0, c: 0, d: 1, tx: 0, ty: 0) }
 }
}

extension CGAffineTransform : Codable {
  public init(from decoder: Decoder) throws {
    var container = try decoder.unkeyedContainer()
    let a = try container.decode(CGFloat.self)
    let b = try container.decode(CGFloat.self)
    let c = try container.decode(CGFloat.self)
    let d = try container.decode(CGFloat.self)
    let tx = try container.decode(CGFloat.self)
    let ty = try container.decode(CGFloat.self)
    self.init(a: a, b: b, c: c, d: d, tx: tx, ty: ty)
  }

  public func encode(to encoder: Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(a)
    try container.encode(b)
    try container.encode(c)
    try container.encode(d)
    try container.encode(tx)
    try container.encode(ty)
  }
}

//===----------------------------------------------------------------------===//
// CGImage
//===----------------------------------------------------------------------===//

extension CGImage {
  public func copy(maskingColorComponents components: [CGFloat]) -> CGImage? {
    return self.__copy(maskingColorComponents: UnsafePointer(components))
  }
}

//===----------------------------------------------------------------------===//
// CGLayer
//===----------------------------------------------------------------------===//

// TODO: remove auxiliaryInfo parameter from CGLayer.init,
// or at least give it a default value (empty/nil)
// blocked on rdar://problem/27444567
extension CGContext {
  public func draw(_ layer: CGLayer, in rect: CGRect) {
    self.__draw(in: rect, layer: layer)
  }

  public func draw(_ layer: CGLayer, at point: CGPoint) {
    self.__draw(at: point, layer: layer)
  }
}

//===----------------------------------------------------------------------===//
// CGPath & CGMutablePath
//===----------------------------------------------------------------------===//

// TODO: Make this a nested type (CGPath.FillRule)
public enum CGPathFillRule: Int {
  /// Nonzero winding number fill rule.
  /// 
  /// This rule plots a ray from the interior of the region to be evaluated
  /// toward the bounds of the drawing, and sums the closed path elements
  /// that the ray crosses: +1 for counterclockwise paths, -1 for clockwise.
  /// If the sum is zero, the region is left empty; if the sum is nonzero,
  /// the region is filled.
  case winding
  
  /// Even-Odd fill rule.
  /// 
  /// This rule plots a ray from the interior of the region to be evaluated
  /// toward the bounds of the drawing, and sums the closed path elements
  /// that the ray crosses.
  /// If the sum is an even numner, the region is left empty; if the sum is
  /// an odd number, the region is filled.
  case evenOdd
}

extension CGPath {
  public func copy(dashingWithPhase phase: CGFloat, lengths: [CGFloat],
   transform: CGAffineTransform = .identity) -> CGPath {
    return CGPath(__byDashing: self, transform: [transform],
     phase: phase, lengths: lengths, count: lengths.count)!
    // force unwrap / non-optional return ok: underlying func returns nil
    // only on bad input that we've made impossible (self and transform)
  }

  public func copy(strokingWithWidth lineWidth: CGFloat, lineCap: CGLineCap,
   lineJoin: CGLineJoin, miterLimit: CGFloat,
   transform: CGAffineTransform = .identity) -> CGPath {
    return CGPath(__byStroking: self, transform: [transform],
     lineWidth: lineWidth, lineCap: lineCap, lineJoin: lineJoin,
     miterLimit: miterLimit)! 
    // force unwrap / non-optional return ok: underlying func returns nil
    // only on bad input that we've made impossible (self and transform)
  }
  
  public func contains(_ point: CGPoint, using rule: CGPathFillRule = .winding,
   transform: CGAffineTransform = .identity) -> Bool {
    return self.__containsPoint(transform: [transform],
     point: point, eoFill: (rule == .evenOdd))
  }
}

extension CGMutablePath {

  public func addRoundedRect(in rect: CGRect, cornerWidth: CGFloat,
   cornerHeight: CGFloat, transform: CGAffineTransform = .identity) {
    self.__addRoundedRect(transform: [transform], rect: rect,
     cornerWidth: cornerWidth, cornerHeight: cornerHeight)
  }

  public func move(to point: CGPoint,
   transform: CGAffineTransform = .identity) {
    self.__moveTo(transform: [transform], x: point.x, y: point.y)
  }

  public func addLine(to point: CGPoint,
   transform: CGAffineTransform = .identity) {
    self.__addLineTo(transform: [transform], x: point.x, y: point.y)
  }

  public func addQuadCurve(to end: CGPoint, control: CGPoint,
   transform: CGAffineTransform = .identity) {
    self.__addQuadCurve(transform: [transform], cpx: control.x, cpy: control.y,
     endingAtX: end.x, y: end.y)
  }

  public func addCurve(to end: CGPoint, control1: CGPoint, control2: CGPoint,
   transform: CGAffineTransform = .identity) {
    self.__addCurve(transform: [transform], cp1x: control1.x, cp1y: control1.y,
     cp2x: control2.x, cp2y: control2.y, endingAtX: end.x, y: end.y)
  }

  public func addRect(_ rect: CGRect,
   transform: CGAffineTransform = .identity) {
    self.__addRect(transform: [transform], rect: rect)
  }

  public func addRects(_ rects: [CGRect],
   transform: CGAffineTransform = .identity) {
    self.__addRects(transform: [transform], rects: rects, count: rects.count)
  }

  public func addLines(between points: [CGPoint],
   transform: CGAffineTransform = .identity) {
    self.__addLines(transform: [transform],
     between: points, count: points.count)
  }

  public func addEllipse(in rect: CGRect,
   transform: CGAffineTransform = .identity) {
    self.__addEllipse(transform: [transform], rect: rect)
  }

  public func addRelativeArc(center: CGPoint, radius: CGFloat,
   startAngle: CGFloat, delta: CGFloat,
   transform: CGAffineTransform = .identity) {
    self.__addRelativeArc(transform: [transform], x: center.x, y: center.y,
     radius: radius, startAngle: startAngle, delta: delta)
  }
  
  public func addArc(center: CGPoint, radius: CGFloat,
   startAngle: CGFloat, endAngle: CGFloat, clockwise: Bool,
   transform: CGAffineTransform = .identity) {
    self.__addArc(transform: [transform], x: center.x, y: center.y,
     radius: radius, startAngle: startAngle, endAngle: endAngle,
     clockwise: clockwise)
  }

  public func addArc(tangent1End: CGPoint, tangent2End: CGPoint,
   radius: CGFloat, transform: CGAffineTransform = .identity) {
    self.__addArc(transform: [transform], x1: tangent1End.x, y1: tangent1End.y,
     x2: tangent2End.x, y2: tangent2End.y, radius: radius)
  }

  public func addPath(_ path: CGPath,
   transform: CGAffineTransform = .identity) {
    self.__addPath(transform: [transform], path: path)
  }
  
}

