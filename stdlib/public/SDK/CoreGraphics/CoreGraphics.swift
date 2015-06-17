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

@exported import CoreGraphics
import Darwin

//===----------------------------------------------------------------------===//
// CGGeometry
//===----------------------------------------------------------------------===//

public extension CGPoint {
  static var zeroPoint: CGPoint {
    @transparent // @fragile
    get { return CGPoint(x: 0, y: 0) }
  }

  @transparent // @fragile
  init(x: Int, y: Int) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }

  @transparent // @fragile
  init(x: Double, y: Double) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }
}

extension CGPoint : Equatable {}
@transparent // @fragile
public func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}


public extension CGSize {
  static var zeroSize: CGSize {
    @transparent // @fragile
    get { return CGSize(width: 0, height: 0) }
  }

  @transparent // @fragile
  init(width: Int, height: Int) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }

  @transparent // @fragile
  init(width: Double, height: Double) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }
}

extension CGSize : Equatable {}
@transparent // @fragile
public func == (lhs: CGSize, rhs: CGSize) -> Bool {
  return lhs.width == rhs.width  &&  lhs.height == rhs.height
}


public extension CGVector {
  static var zeroVector: CGVector {
    @transparent // @fragile
    get { return CGVector(dx: 0, dy: 0) }
  }

  @transparent // @fragile
  init(dx: Int, dy: Int) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }

  @transparent // @fragile
  init(dx: Double, dy: Double) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }
}

extension CGVector : Equatable {}
@transparent // @fragile
public func == (lhs: CGVector, rhs: CGVector) -> Bool {
  return lhs.dx == rhs.dx  &&  lhs.dy == rhs.dy
}


public extension CGRect {
  static var zeroRect:     CGRect {
    @transparent // @fragile
    get { return CGRect(x: 0, y: 0, width: 0, height: 0) }
  }
  static var nullRect:     CGRect {
    @transparent // @fragile
    get { return CGRectNull }
  }
  static var infiniteRect: CGRect {
    @transparent // @fragile
    get { return CGRectInfinite }
  }

  @transparent // @fragile
  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  @transparent // @fragile
  init(x: Double, y: Double, width: Double, height: Double) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  @transparent // @fragile
  init(x: Int, y: Int, width: Int, height: Int) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  var width:  CGFloat {
    @transparent // @fragile
    get { return CGRectGetWidth(self) }
  }
  var height: CGFloat {
    @transparent // @fragile
    get { return CGRectGetHeight(self) }
  }

  var minX:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMinX(self) }
  }
  var midX:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMidX(self) }
  }
  var maxX:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMaxX(self) }
  }

  var minY:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMinY(self) }
  }
  var midY:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMidY(self) }
  }
  var maxY:   CGFloat {
    @transparent // @fragile
    get { return CGRectGetMaxY(self) }
  }

  var isNull:     Bool {
    @transparent // @fragile
    get { return CGRectIsNull(self) }
  }
  var isEmpty:    Bool {
    @transparent // @fragile
    get { return CGRectIsEmpty(self) }
  }
  var isInfinite: Bool {
    @transparent // @fragile
    get { return CGRectIsInfinite(self) }
  }

  var standardizedRect: CGRect { 
    @transparent // @fragile
    get { return CGRectStandardize(self) } 
  }

  @transparent // @fragile
  mutating func standardize() {
    self = self.standardizedRect
  }

  var integerRect: CGRect {
    @transparent // @fragile
    get { return CGRectIntegral(self) }
  }

  @transparent // @fragile
  mutating func integerize() {
    self = self.integerRect
  }


  @transparent // @fragile
  func rectByInsetting(dx dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectInset(self, dx, dy)
  }

  @transparent // @fragile
  mutating func inset(dx dx: CGFloat, dy: CGFloat) {
    self = self.rectByInsetting(dx: dx, dy: dy)
  }


  @transparent // @fragile
  func rectByOffsetting(dx dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectOffset(self, dx, dy)
  }

  @transparent // @fragile
  mutating func offset(dx dx: CGFloat, dy: CGFloat) {
    self = self.rectByOffsetting(dx: dx, dy: dy)
  }


  @transparent // @fragile
  func rectByUnion(withRect: CGRect) -> CGRect {
    return CGRectUnion(self, withRect)
  }
  
  @transparent // @fragile
  mutating func union(withRect: CGRect) {
    self = self.rectByUnion(withRect)
  }

  @transparent // @fragile
  func rectByIntersecting(withRect: CGRect) -> CGRect {
    return CGRectIntersection(self, withRect)
  }
  
  @transparent // @fragile
  mutating func intersect(withRect: CGRect) {
    self = self.rectByIntersecting(withRect)
  }


  @transparent // @fragile
  func rectsByDividing(atDistance: CGFloat, fromEdge: CGRectEdge) 
    -> (slice: CGRect, remainder: CGRect) 
  {
    var slice = CGRect.zeroRect
    var remainder = CGRect.zeroRect
    CGRectDivide(self, &slice, &remainder, atDistance, fromEdge)
    return (slice, remainder)
  }

  
  @transparent // @fragile
  func contains(rect: CGRect) -> Bool {
    return CGRectContainsRect(self, rect)
  }

  @transparent // @fragile
  func contains(point: CGPoint) -> Bool {
    return CGRectContainsPoint(self, point)
  }

  @transparent // @fragile
  func intersects(rect: CGRect) -> Bool {
    return CGRectIntersectsRect(self, rect)
  }
}

extension CGRect : Equatable {}
@transparent // @fragile
public func == (lhs: CGRect, rhs: CGRect) -> Bool {
  return CGRectEqualToRect(lhs, rhs)
}

// Overlay the C names of these constants with transparent definitions. The
// C constants are opaque extern globals for no good reason.

public var CGPointZero: CGPoint {
  @transparent // @fragile
  get { return CGPoint.zeroPoint }
}

public var CGRectZero: CGRect {
  @transparent // @fragile
  get { return CGRect.zeroRect }
}

public var CGSizeZero: CGSize {
  @transparent // @fragile
  get { return CGSize.zeroSize }
}

public var CGAffineTransformIdentity: CGAffineTransform {
  @transparent // @fragile
  get { return CGAffineTransform(a: 1, b: 0, c: 0, d: 1, tx: 0, ty: 0) }
}
