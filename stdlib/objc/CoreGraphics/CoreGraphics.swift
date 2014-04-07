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


//===----------------------------------------------------------------------===//
// CGGeometry
//===----------------------------------------------------------------------===//

extension CGPoint : Equatable {
  static var zeroPoint: CGPoint { get { return CGPoint(0, 0) } }
}

func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}


extension CGSize : Equatable {
  static var zeroSize: CGSize { get { return CGSize(0, 0) } }
}

func == (lhs: CGSize, rhs: CGSize) -> Bool {
  return lhs.width == rhs.width  &&  lhs.height == rhs.height
}


extension CGVector : Equatable {
  static var zeroVector: CGVector { get { return CGVector(0, 0) } }
}

func == (lhs: CGVector, rhs: CGVector) -> Bool {
  return lhs.dx == rhs.dx  &&  lhs.dy == rhs.dy
}


extension CGRect : Equatable {
  static var zeroRect:     CGRect { get { return CGRect(0, 0, 0, 0) } }
  static var nullRect:     CGRect { get { return CGRectNull } }
  static var infiniteRect: CGRect { get { return CGRectInfinite } }

  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {
    self.init(origin:CGPoint(x, y), size:CGSize(width, height))
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

  @mutating func standardize() {
    self = self.standardizedRect
  }

  var integerRect: CGRect {
    get { return CGRectIntegral(self) }
  }

  @mutating func integerize() {
    self = self.integerRect
  }


  func rectByInsetting(dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectInset(self, dx, dy)
  }

  @mutating func inset(dx: CGFloat, dy: CGFloat) {
    self = self.rectByInsetting(dx, dy)
  }


  func rectByOffsetting(dx: CGFloat, dy: CGFloat) -> CGRect {
    return CGRectOffset(self, dx, dy)
  }

  @mutating func offset(dx: CGFloat, dy: CGFloat) {
    self = self.rectByOffsetting(dx, dy)
  }


  func rectByUnion(withRect: CGRect) -> CGRect {
    return CGRectUnion(self, withRect)
  }
  
  @mutating func union(withRect: CGRect) {
    self = self.rectByUnion(withRect)
  }

  func rectByIntersecting(withRect: CGRect) -> CGRect {
    return CGRectIntersection(self, withRect)
  }
  
  @mutating func intersect(withRect: CGRect) {
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

func == (lhs: CGRect, rhs: CGRect) -> Bool {
  return CGRectEqualToRect(lhs, rhs)
}

