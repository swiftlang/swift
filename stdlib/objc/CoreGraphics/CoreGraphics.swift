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
  static var zeroPoint: CGPoint { get { return CGPoint(x: 0, y: 0) } }

  init() {
    self.init(x: 0.0, y: 0.0)
  }

  init(x: Int, y: Int) {
    self.init(x: CGFloat(x), y: CGFloat(y))
  }
}

struct _CGPointMirror : Mirror {
  let _value : CGPoint

  init(_ x : CGPoint) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 2 }

  subscript(i: Int) -> (String, Mirror) { 
    switch i {
    case 0: return ("x",reflect(_value.x))
    case 1: return ("y",reflect(_value.y))
    default: fatal("cannot extract this child index")
    }
  }

  var summary: String { return "(\(_value.x),\(_value.y))" }

  var quickLookObject: QuickLookObject? { return .Some(.Point(Double(_value.x),Double(_value.y))) }

  var disposition: MirrorDisposition { return .Aggregate }
}

extension CGPoint : Reflectable {
  func getMirror() -> Mirror {
    return _CGPointMirror(self)
  }
}

func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}


extension CGSize : Equatable {
  static var zeroSize: CGSize { return CGSize(width: 0, height: 0) }

  init() {
    self.init(width: 0.0, height: 0.0)
  }

  init(width: Int, height: Int) {
    self.init(width: CGFloat(width), height: CGFloat(height))
  }
}

struct _CGSizeMirror : Mirror {
  let _value : CGSize

  init(_ x : CGSize) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 2 }

  subscript(i: Int) -> (String, Mirror) { 
    switch i {
    case 0: return ("x",reflect(_value.width))
    case 1: return ("y",reflect(_value.height))
    default: fatal("cannot extract this child index")
    }
  }

  var summary: String { return "(\(_value.width),\(_value.height))" }

  var quickLookObject: QuickLookObject? { return .Some(.Size(Double(_value.width),Double(_value.height))) }

  var disposition: MirrorDisposition { return .Aggregate }
}

extension CGSize : Reflectable {
  func getMirror() -> Mirror {
    return _CGSizeMirror(self)
  }
}

func == (lhs: CGSize, rhs: CGSize) -> Bool {
  return lhs.width == rhs.width  &&  lhs.height == rhs.height
}


extension CGVector : Equatable {
  static var zeroVector: CGVector { get { return CGVector(0, 0) } }

  init(_ dx: CGFloat, _ dy: CGFloat) {
    self.init(dx: CGFloat(dx), dy: CGFloat(dy))
  }

  init(_ dx: Int, _ dy: Int) {
    self.init(CGFloat(dx), CGFloat(dy))
  }
}

func == (lhs: CGVector, rhs: CGVector) -> Bool {
  return lhs.dx == rhs.dx  &&  lhs.dy == rhs.dy
}


extension CGRect : Equatable {
  static var zeroRect:     CGRect { 
    return CGRect(x: 0, y: 0, width: 0, height: 0) 
  }
  static var nullRect:     CGRect { get { return CGRectNull } }
  static var infiniteRect: CGRect { get { return CGRectInfinite } }

  init() {
    self.init(origin: CGPoint(), size: CGSize())
  }

  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {
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

struct _CGRectMirror : Mirror {
  let _value : CGRect

  init(_ x : CGRect) {
    _value = x
  }

  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 2 }

  subscript(i: Int) -> (String, Mirror) { 
    switch i {
    case 0: return ("x",reflect(_value.origin))
    case 1: return ("y",reflect(_value.size))
    default: fatal("cannot extract this child index")
    }
  }

  var summary: String { return "(\(_value.origin.x),\(_value.origin.y),\(_value.size.width),\(_value.size.height))" }

  var quickLookObject: QuickLookObject? { return .Some(.Rectangle(Double(_value.origin.x),Double(_value.origin.y),Double(_value.size.width),Double(_value.size.height))) }

  var disposition: MirrorDisposition { return .Aggregate }
}

extension CGRect : Reflectable {
  func getMirror() -> Mirror {
    return _CGRectMirror(self)
  }
}

func == (lhs: CGRect, rhs: CGRect) -> Bool {
  return CGRectEqualToRect(lhs, rhs)
}

