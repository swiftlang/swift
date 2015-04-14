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

@exported import SceneKit // Clang module

@availability(iOS, introduced=8.0)
@availability(OSX, introduced=10.8)
extension SCNGeometryElement {
  public convenience init<IndexType : IntegerType>(
    indices: [IndexType], primitiveType: SCNGeometryPrimitiveType
  ) {
    let indexCount = indices.count
    let primitiveCount: Int
    switch primitiveType {
    case .Triangles:
      primitiveCount = indexCount / 3
    case .TriangleStrip:
      primitiveCount = indexCount - 2
    case .Line:
      primitiveCount = indexCount / 2
    case .Point:
      primitiveCount = indexCount
    }
    self.init(
      data: NSData(bytes: indices, length: indexCount * sizeof(IndexType)),
      primitiveType: primitiveType,
      primitiveCount: primitiveCount,
      bytesPerIndex: sizeof(IndexType))
  }
}

@availability(iOS, introduced=8.0)
@availability(OSX, introduced=10.8)
extension SCNSceneSource {
  public func entryWithIdentifier<T>(uid: String, withClass entryClass: T.Type) -> T? {
    return entryWithIdentifier(uid, withClass: entryClass as! AnyClass) as! T?
  }
}

