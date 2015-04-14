// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

import StdlibUnittest
import SceneKit

// SceneKit is only available on iOS 8.0 and above and on OS X 10.8 and above.

var SceneKitTests = TestSuite("SceneKit")

func bytesFromNSData(data: NSData) -> [UInt8] {
  return Array(UnsafeBufferPointer(
    start: UnsafePointer<UInt8>(data.bytes),
    count: data.length))
}

if #available(iOS >= 8.0, OSX >= 10.8, *) {
  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/Int") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, Int.max, 4, 5, 6 ],
      primitiveType: .Triangles)

    expectEqual(.Triangles, element.primitiveType)
    expectEqual(2, element.primitiveCount)
  #if arch(i386) || arch(arm)
    expectEqual(
      [
        1,0,0,0,
        2,0,0,0,
        0xff,0xff,0xff,0x7f,
        4,0,0,0,
        5,0,0,0,
        6,0,0,0,
      ],
      bytesFromNSData(element.data))
    expectEqual(4, element.bytesPerIndex)
  #elseif arch(x86_64) || arch(arm64)
    expectEqual(
      [
        1,0,0,0, 0,0,0,0,
        2,0,0,0, 0,0,0,0,
        0xff,0xff,0xff,0xff, 0xff,0xff,0xff,0x7f,
        4,0,0,0, 0,0,0,0,
        5,0,0,0, 0,0,0,0,
        6,0,0,0, 0,0,0,0,
      ],
      bytesFromNSData(element.data))
    expectEqual(8, element.bytesPerIndex)
  #else
    _portThisCode()
  #endif
  }

  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/Int16") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, Int16.max, Int16.max/2, 5, 6 ] as [Int16],
      primitiveType: .Triangles)

    expectEqual(.Triangles, element.primitiveType)
    expectEqual(2, element.primitiveCount)
    expectEqual(
      [
        1, 0,
        2, 0,
        0xff, 0x7f,
        0xff, 0x3f,
        5, 0,
        6, 0
      ],
      bytesFromNSData(element.data))
    expectEqual(2, element.bytesPerIndex)
  }

  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/Triangles") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, UInt8.max, UInt8.max/2, 5, 6 ] as [UInt8],
      primitiveType: .Triangles)

    expectEqual(.Triangles, element.primitiveType)
    expectEqual(2, element.primitiveCount)
    expectEqual(
      [ 1, 2, UInt8.max, UInt8.max/2, 5, 6 ],
      bytesFromNSData(element.data))
    expectEqual(1, element.bytesPerIndex)
  }

  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/TriangleStrip") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, 3, 4, 5, 6 ] as [UInt8],
      primitiveType: .TriangleStrip)

    expectEqual(.TriangleStrip, element.primitiveType)
    expectEqual(4, element.primitiveCount)
    expectEqual(
      [ 1, 2, 3, 4, 5, 6 ],
      bytesFromNSData(element.data))
    expectEqual(1, element.bytesPerIndex)
  }

  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/Line") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, 3, 4, 5, 6 ] as [UInt8],
      primitiveType: .Line)

    expectEqual(.Line, element.primitiveType)
    expectEqual(3, element.primitiveCount)
    expectEqual(
      [ 1, 2, 3, 4, 5, 6 ],
      bytesFromNSData(element.data))
    expectEqual(1, element.bytesPerIndex)
  }

  SceneKitTests.test("SCNGeometryElement.init(indices:primitiveType:)/Point") {
    let element = SCNGeometryElement(
      indices: [ 1, 2, 3, 4, 5, 6 ] as [UInt8],
      primitiveType: .Point)

    expectEqual(.Point, element.primitiveType)
    expectEqual(6, element.primitiveCount)
    expectEqual(
      [ 1, 2, 3, 4, 5, 6 ],
      bytesFromNSData(element.data))
    expectEqual(1, element.bytesPerIndex)
  }

  // Compile-only test, don't know how to instantiate SCNSceneSource.
  func test_SCNSceneSource_entryWithIdentifier(source: SCNSceneSource) {
    if true {
      var result = source.entryWithIdentifier("abc", withClass: SCNScene.self)
      // FIXME: wrong.  Need to mark the original declaration unavaliable.
      expectType(Optional<AnyObject>.self, &result)
      // expectType(Optional<SCNScene>.self, &result)
    }
    if true {
      var result = source.entryWithIdentifier("abc", withClass: SCNNode.self)
      // FIXME: wrong.  Need to mark the original declaration unavaliable.
      expectType(Optional<AnyObject>.self, &result)
      // expectType(Optional<SCNScene>.self, &result)
    }
  }
}

runAllTests()

