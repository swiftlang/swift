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

@_exported import SceneKit // Clang module

// MARK: Exposing SCNFloat

#if os(OSX)
public typealias SCNFloat = CGFloat
#elseif os(iOS) || os(tvOS)
public typealias SCNFloat = Float
#endif

// MARK: Working with SCNVector3

extension SCNVector3 {
  public init(_ x: Float, _ y: Float, _ z: Float) {
    self.x = SCNFloat(x)
    self.y = SCNFloat(y)
    self.z = SCNFloat(z)
  }
  public init(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat) {
    self.x = SCNFloat(x)
    self.y = SCNFloat(y)
    self.z = SCNFloat(z)
  }
  public init(_ x: Double, _ y: Double, _ z: Double) {
    self.init(SCNFloat(x), SCNFloat(y), SCNFloat(z))
  }
  public init(_ x: Int, _ y: Int, _ z: Int) {
    self.init(SCNFloat(x), SCNFloat(y), SCNFloat(z))
  }
  public init(_ v: float3) {
    self.init(SCNFloat(v.x), SCNFloat(v.y), SCNFloat(v.z))
  }
  public init(_ v: double3) {
    self.init(SCNFloat(v.x), SCNFloat(v.y), SCNFloat(v.z))
  }
}

extension float3 {
  public init(_ v: SCNVector3) {
    self.init(Float(v.x), Float(v.y), Float(v.z))
  }
}

extension double3 {
  public init(_ v: SCNVector3) {
    self.init(Double(v.x), Double(v.y), Double(v.z))
  }
}

// MARK: Working with SCNVector4

extension SCNVector4 {
  public init(_ x: Float, _ y: Float, _ z: Float, _ w: Float) {
    self.x = SCNFloat(x)
    self.y = SCNFloat(y)
    self.z = SCNFloat(z)
    self.w = SCNFloat(w)
  }
  public init(_ x: CGFloat, _ y: CGFloat, _ z: CGFloat, _ w: CGFloat) {
    self.x = SCNFloat(x as NSNumber)
    self.y = SCNFloat(y as NSNumber)
    self.z = SCNFloat(z as NSNumber)
    self.w = SCNFloat(w as NSNumber)
  }
  public init(_ x: Double, _ y: Double, _ z: Double, _ w: Double) {
    self.init(SCNFloat(x), SCNFloat(y), SCNFloat(z), SCNFloat(w))
  }
  public init(_ x: Int, _ y: Int, _ z: Int, _ w: Int) {
    self.init(SCNFloat(x), SCNFloat(y), SCNFloat(z), SCNFloat(w))
  }
  public init(_ v: float4) {
    self.init(SCNFloat(v.x), SCNFloat(v.y), SCNFloat(v.z), SCNFloat(v.w))
  }
  public init(_ v: double4) {
    self.init(SCNFloat(v.x), SCNFloat(v.y), SCNFloat(v.z), SCNFloat(v.w))
  }
}

extension float4 {
  public init(_ v: SCNVector4) {
    self.init(Float(v.x), Float(v.y), Float(v.z), Float(v.w))
  }
}

extension double4 {
  public init(_ v: SCNVector4) {
    self.init(Double(v.x), Double(v.y), Double(v.z), Double(v.w))
  }
}

// MARK: Working with SCNMatrix4

extension SCNMatrix4 {
  public init(_ m: float4x4) {
    self.init(
      m11: SCNFloat(m[0,0]), m12: SCNFloat(m[0,1]), m13: SCNFloat(m[0,2]), m14: SCNFloat(m[0,3]),
      m21: SCNFloat(m[1,0]), m22: SCNFloat(m[1,1]), m23: SCNFloat(m[1,2]), m24: SCNFloat(m[1,3]),
      m31: SCNFloat(m[2,0]), m32: SCNFloat(m[2,1]), m33: SCNFloat(m[2,2]), m34: SCNFloat(m[2,3]),
      m41: SCNFloat(m[3,0]), m42: SCNFloat(m[3,1]), m43: SCNFloat(m[3,2]), m44: SCNFloat(m[3,3]))
  }
  public init(_ m: double4x4) {
    self.init(
      m11: SCNFloat(m[0,0]), m12: SCNFloat(m[0,1]), m13: SCNFloat(m[0,2]), m14: SCNFloat(m[0,3]),
      m21: SCNFloat(m[1,0]), m22: SCNFloat(m[1,1]), m23: SCNFloat(m[1,2]), m24: SCNFloat(m[1,3]),
      m31: SCNFloat(m[2,0]), m32: SCNFloat(m[2,1]), m33: SCNFloat(m[2,2]), m34: SCNFloat(m[2,3]),
      m41: SCNFloat(m[3,0]), m42: SCNFloat(m[3,1]), m43: SCNFloat(m[3,2]), m44: SCNFloat(m[3,3]))
  }
}

extension float4x4 {
  public init(_ m: SCNMatrix4) {
    self.init([
      float4(Float(m.m11), Float(m.m12), Float(m.m13), Float(m.m14)),
      float4(Float(m.m21), Float(m.m22), Float(m.m23), Float(m.m24)),
      float4(Float(m.m31), Float(m.m32), Float(m.m33), Float(m.m34)),
      float4(Float(m.m41), Float(m.m42), Float(m.m43), Float(m.m44))
      ])
  }
}

extension double4x4 {
  public init(_ m: SCNMatrix4) {
    self.init([
      double4(Double(m.m11), Double(m.m12), Double(m.m13), Double(m.m14)),
      double4(Double(m.m21), Double(m.m22), Double(m.m23), Double(m.m24)),
      double4(Double(m.m31), Double(m.m32), Double(m.m33), Double(m.m34)),
      double4(Double(m.m41), Double(m.m42), Double(m.m43), Double(m.m44))
      ])
  }
}

// MARK: APIs refined for Swift

@available(iOS, introduced: 8.0)
@available(OSX, introduced: 10.8)
extension SCNGeometryElement {
  public convenience init<IndexType : Integer>(
    indices: [IndexType], primitiveType: SCNGeometryPrimitiveType
  ) {
    let indexCount = indices.count
    let primitiveCount: Int
    switch primitiveType {
    case .triangles:
      primitiveCount = indexCount / 3
    case .triangleStrip:
      primitiveCount = indexCount - 2
    case .line:
      primitiveCount = indexCount / 2
    case .point:
      primitiveCount = indexCount
    }
    self.init(
      data: NSData(bytes: indices, length: indexCount * sizeof(IndexType)),
      primitiveType: primitiveType,
      primitiveCount: primitiveCount,
      bytesPerIndex: sizeof(IndexType))
  }
}

@warn_unused_result
@_silgen_name("SCN_Swift_SCNSceneSource_entryWithIdentifier")
internal func SCN_Swift_SCNSceneSource_entryWithIdentifier(
  _ self_: AnyObject,
  _ uid: NSString,
  _ entryClass: AnyObject) -> AnyObject?

@available(iOS, introduced: 8.0)
@available(OSX, introduced: 10.8)
extension SCNSceneSource {
  @warn_unused_result
  public func entryWithIdentifier<T>(_ uid: String, withClass entryClass: T.Type) -> T? {
    return SCN_Swift_SCNSceneSource_entryWithIdentifier(
      self, uid as NSString, entryClass as! AnyObject) as! T?
  }
}

