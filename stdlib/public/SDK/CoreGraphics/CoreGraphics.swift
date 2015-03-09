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
  static var zeroPoint: CGPoint { get { return CGPoint(x: 0, y: 0) } }

  init(x: Int, y: Int) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }

  init(x: Double, y: Double) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }
}

extension CGPoint : Equatable {}
public func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}


public extension CGSize {
  static var zeroSize: CGSize { return CGSize(width: 0, height: 0) }

  init(width: Int, height: Int) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }

  init(width: Double, height: Double) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }
}

extension CGSize : Equatable {}
public func == (lhs: CGSize, rhs: CGSize) -> Bool {
  return lhs.width == rhs.width  &&  lhs.height == rhs.height
}


public extension CGVector {
  static var zeroVector: CGVector { get { return CGVector(dx: 0, dy: 0) } }

  init(dx: Int, dy: Int) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }

  init(dx: Double, dy: Double) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }
}

extension CGVector : Equatable {}
public func == (lhs: CGVector, rhs: CGVector) -> Bool {
  return lhs.dx == rhs.dx  &&  lhs.dy == rhs.dy
}


public extension CGRect {
  static var zeroRect:     CGRect {
    return CGRect(x: 0, y: 0, width: 0, height: 0) 
  }
  static var nullRect:     CGRect { get { return CGRectNull } }
  static var infiniteRect: CGRect { get { return CGRectInfinite } }

  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  init(x: Double, y: Double, width: Double, height: Double) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  init(x: Int, y: Int, width: Int, height: Int) {
    self.init(origin: CGPoint(x: x, y: y), 
              size: CGSize(width: width, height: height))
  }

  var width:  CGFloat { get { return CGRectGetWidth(self) } }
  var height: CGFloat { get { return CGRectGetHeight(self) } }

  var minX:   CGFloat { get { return CGRectGetMinX(self) } }
  var midX:   CGFloat { get { return CGRectGetMidX(self) } }
  var maxX:   CGFloat { get { return CGRectGetMaxX(self) } }

  var minY:   CGFloat { get { return CGRectGetMinY(self) } }
  var midY:   CGFloat { get { return CGRectGetMidY(self) } }
  var maxY:   CGFloat { get { return CGRectGetMaxY(self) } }

  var isNull:     Bool { get { return CGRectIsNull(self) } }
  var isEmpty:    Bool { get { return CGRectIsEmpty(self) } }
  var isInfinite: Bool { get { return CGRectIsInfinite(self) } }

  var standardizedRect: CGRect { 
    get { return CGRectStandardize(self) } 
  }

  mutating func standardize() {
    self = self.standardizedRect
  }

  var integerRect: CGRect {
    get { return CGRectIntegral(self) }
  }

  mutating func integerize() {
    self = self.integerRect
  }


  func rectByInsetting(#dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectInset(self, dx, dy)
  }

  mutating func inset(#dx: CGFloat, dy: CGFloat) {
    self = self.rectByInsetting(dx: dx, dy: dy)
  }


  func rectByOffsetting(#dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectOffset(self, dx, dy)
  }

  mutating func offset(#dx: CGFloat, dy: CGFloat) {
    self = self.rectByOffsetting(dx: dx, dy: dy)
  }


  func rectByUnion(withRect: CGRect) -> CGRect {
    return CGRectUnion(self, withRect)
  }
  
  mutating func union(withRect: CGRect) {
    self = self.rectByUnion(withRect)
  }

  func rectByIntersecting(withRect: CGRect) -> CGRect {
    return CGRectIntersection(self, withRect)
  }
  
  mutating func intersect(withRect: CGRect) {
    self = self.rectByIntersecting(withRect)
  }


  func rectsByDividing(atDistance: CGFloat, fromEdge: CGRectEdge) 
    -> (slice: CGRect, remainder: CGRect) 
  {
    var slice = CGRect.zeroRect
    var remainder = CGRect.zeroRect
    CGRectDivide(self, &slice, &remainder, atDistance, fromEdge)
    return (slice, remainder)
  }

  
  func contains(rect: CGRect) -> Bool {
    return CGRectContainsRect(self, rect)
  }

  func contains(point: CGPoint) -> Bool {
    return CGRectContainsPoint(self, point)
  }

  func intersects(rect: CGRect) -> Bool {
    return CGRectIntersectsRect(self, rect)
  }
}

extension CGRect : Equatable {}
public func == (lhs: CGRect, rhs: CGRect) -> Bool {
  return CGRectEqualToRect(lhs, rhs)
}

