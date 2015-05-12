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

if #available(iOS 8.0, *) {
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

  SceneKitTests.test("SCNSceneSource.entryWithIdentifier(uid:withClass:)") {
    let sceneDescription =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "<COLLADA xmlns=\"http://www.collada.org/2005/11/COLLADASchema\" version=\"1.4.1\">" +
      " <library_materials>" +
      "  <material id=\"material1\">" +
      "   <instance_effect url=\"#effect_material1\"/>" +
      "  </material>" +
      "  <material id=\"material2\">" +
      "   <instance_effect url=\"#effect_material2\"/>" +
      "  </material>" +
      " </library_materials>" +
      " <library_effects>" +
      "  <effect id=\"effect_material1\">" +
      "   <profile_COMMON>" +
      "    <technique sid=\"common\">" +
      "     <blinn>" +
      "      <shininess>" +
      "       <float>0.022516</float>" +
      "      </shininess>" +
      "      <transparency>" +
      "       <float>1</float>" +
      "      </transparency>" +
      "      <index_of_refraction>" +
      "       <float>1</float>" +
      "      </index_of_refraction>" +
      "     </blinn>" +
      "    </technique>" +
      "   </profile_COMMON>" +
      "  </effect>" +
      "  <effect id=\"effect_material2\">" +
      "   <profile_COMMON>" +
      "    <technique sid=\"common\">" +
      "     <blinn>" +
      "      <shininess>" +
      "       <float>0.022516</float>" +
      "      </shininess>" +
      "      <transparency>" +
      "       <float>1</float>" +
      "      </transparency>" +
      "      <index_of_refraction>" +
      "       <float>1</float>" +
      "      </index_of_refraction>" +
      "     </blinn>" +
      "    </technique>" +
      "   </profile_COMMON>" +
      "  </effect>" +
      " </library_effects>" +
      " <library_geometries>" +
      "  <geometry id=\"plane\">" +
      "   <mesh>" +
      "    <source id=\"geometrySource4\">" +
      "     <float_array id=\"ID5-array\" count=\"12\">-5 -5 0 5 -5 0 -5 5 0 5 5 0 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID5-array\" count=\"4\" stride=\"3\">" +
      "       <param name=\"X\" type=\"float\"/>" +
      "       <param name=\"Y\" type=\"float\"/>" +
      "       <param name=\"Z\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <source id=\"geometrySource6\">" +
      "     <float_array id=\"ID7-array\" count=\"12\">0 0 1 0 0 1 0 0 1 0 0 1 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID7-array\" count=\"4\" stride=\"3\">" +
      "       <param name=\"X\" type=\"float\"/>" +
      "       <param name=\"Y\" type=\"float\"/>" +
      "       <param name=\"Z\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <source id=\"geometrySource8\">" +
      "     <float_array id=\"ID9-array\" count=\"8\">0 0 1 0 0 1 1 1 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID9-array\" count=\"4\" stride=\"2\">" +
      "       <param name=\"S\" type=\"float\"/>" +
      "       <param name=\"T\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <vertices id=\"geometrySource4-vertices\">" +
      "     <input semantic=\"POSITION\" source=\"#geometrySource4\"/>" +
      "     <input semantic=\"NORMAL\" source=\"#geometrySource6\"/>" +
      "    </vertices>" +
      "    <triangles count=\"2\" material=\"geometryElement10\">" +
      "     <input semantic=\"VERTEX\" offset=\"0\" source=\"#geometrySource4-vertices\"/>" +
      "     <input semantic=\"NORMAL\" offset=\"0\" source=\"#geometrySource6\"/>" +
      "     <input semantic=\"TEXCOORD\" offset=\"0\" source=\"#geometrySource8\" set=\"1\"/>" +
      "     <p>0 1 3 0 3 2 </p>" +
      "    </triangles>" +
      "   </mesh>" +
      "  </geometry>" +
      "  <geometry id=\"box\">" +
      "   <mesh>" +
      "    <source id=\"geometrySource12\">" +
      "     <float_array id=\"ID13-array\" count=\"72\">-2.5 -2.5 2.5 -2.5 2.5 2.5 2.5 -2.5 2.5 2.5 2.5 2.5 2.5 -2.5 2.5 2.5 2.5 2.5 2.5 -2.5 -2.5 2.5 2.5 -2.5 2.5 -2.5 -2.5 2.5 2.5 -2.5 -2.5 -2.5 -2.5 -2.5 2.5 -2.5 -2.5 -2.5 -2.5 -2.5 2.5 -2.5 -2.5 -2.5 2.5 -2.5 2.5 2.5 -2.5 2.5 2.5 -2.5 2.5 -2.5 2.5 2.5 2.5 2.5 2.5 -2.5 -2.5 -2.5 -2.5 -2.5 -2.5 2.5 2.5 -2.5 -2.5 2.5 -2.5 2.5 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID13-array\" count=\"24\" stride=\"3\">" +
      "       <param name=\"X\" type=\"float\"/>" +
      "       <param name=\"Y\" type=\"float\"/>" +
      "       <param name=\"Z\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <source id=\"geometrySource14\">" +
      "     <float_array id=\"ID15-array\" count=\"72\">0 0 1 0 0 1 0 0 1 0 0 1 1 0 -4.37114e-08 1 0 -4.37114e-08 1 0 -4.37114e-08 1 0 -4.37114e-08 -8.74228e-08 0 -1 -8.74228e-08 0 -1 -8.74228e-08 0 -1 -8.74228e-08 0 -1 -1 0 1.19249e-08 -1 0 1.19249e-08 -1 0 1.19249e-08 -1 0 1.19249e-08 0 1 -4.37114e-08 0 1 -4.37114e-08 0 1 -4.37114e-08 0 1 -4.37114e-08 0 -1 -4.37114e-08 0 -1 -4.37114e-08 0 -1 -4.37114e-08 0 -1 -4.37114e-08 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID15-array\" count=\"24\" stride=\"3\">" +
      "       <param name=\"X\" type=\"float\"/>" +
      "       <param name=\"Y\" type=\"float\"/>" +
      "       <param name=\"Z\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <source id=\"geometrySource16\">" +
      "     <float_array id=\"ID17-array\" count=\"48\">0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 </float_array>" +
      "     <technique_common>" +
      "      <accessor source=\"#ID17-array\" count=\"24\" stride=\"2\">" +
      "       <param name=\"S\" type=\"float\"/>" +
      "       <param name=\"T\" type=\"float\"/>" +
      "      </accessor>" +
      "     </technique_common>" +
      "    </source>" +
      "    <vertices id=\"geometrySource12-vertices\">" +
      "     <input semantic=\"POSITION\" source=\"#geometrySource12\"/>" +
      "     <input semantic=\"NORMAL\" source=\"#geometrySource14\"/>" +
      "    </vertices>" +
      "    <triangles count=\"12\" material=\"geometryElement18\">" +
      "     <input semantic=\"VERTEX\" offset=\"0\" source=\"#geometrySource12-vertices\"/>" +
      "     <input semantic=\"NORMAL\" offset=\"0\" source=\"#geometrySource14\"/>" +
      "     <input semantic=\"TEXCOORD\" offset=\"0\" source=\"#geometrySource16\" set=\"1\"/>" +
      "     <p>0 3 1 0 2 3 4 7 5 4 6 7 8 11 9 8 10 11 12 15 13 12 14 15 16 19 17 16 18 19 20 23 21 20 22 23 </p>" +
      "    </triangles>" +
      "   </mesh>" +
      "  </geometry>" +
      " </library_geometries>" +
      " <library_visual_scenes>" +
      "  <visual_scene id=\"scene19\">" +
      "   <node id=\"plane_node\" name=\"plane\">" +
      "    <instance_geometry url=\"#plane\">" +
      "     <bind_material>" +
      "      <technique_common>" +
      "       <instance_material symbol=\"geometryElement10\" target=\"#material1\"/>" +
      "      </technique_common>" +
      "     </bind_material>" +
      "    </instance_geometry>" +
      "   </node>" +
      "   <node id=\"box-node\" name=\"box-node\">" +
      "    <instance_geometry url=\"#box\">" +
      "     <bind_material>" +
      "      <technique_common>" +
      "       <instance_material symbol=\"geometryElement18\" target=\"#material2\"/>" +
      "      </technique_common>" +
      "     </bind_material>" +
      "    </instance_geometry>" +
      "   </node>" +
      "  </visual_scene>" +
      " </library_visual_scenes>" +
      " <scene>" +
      "  <instance_visual_scene url=\"#scene19\"/>" +
      " </scene>" +
      "</COLLADA>"

    let sceneData = sceneDescription.dataUsingEncoding(
      NSUTF8StringEncoding,
      allowLossyConversion: true)!
    let sceneSource = SCNSceneSource(data: sceneData, options: nil)

    if true {
      var unarchivedPlaneGeometry =
        sceneSource.entryWithIdentifier("plane", withClass: SCNGeometry.self)
      var unarchivedPlaneNode_nil =
        sceneSource.entryWithIdentifier("plane-node", withClass: SCNNode.self)

      expectNotEmpty(unarchivedPlaneGeometry)
      expectType(Optional<SCNGeometry>.self, &unarchivedPlaneGeometry)

      expectEmpty(unarchivedPlaneNode_nil)
    }

    if true {
      var unarchivedBoxGeometry =
        sceneSource.entryWithIdentifier("box", withClass: SCNGeometry.self)
      var unarchivedBoxGeometry_nil =
        sceneSource.entryWithIdentifier("box-node", withClass: SCNGeometry.self)

      expectNotEmpty(unarchivedBoxGeometry)
      expectType(Optional<SCNGeometry>.self, &unarchivedBoxGeometry)

      expectEmpty(unarchivedBoxGeometry_nil)
    }

    if true {
      var unarchivedBoxNode =
        sceneSource.entryWithIdentifier("box-node", withClass: SCNNode.self)
      var unarchivedBoxNode_nil =
        sceneSource.entryWithIdentifier("box", withClass: SCNNode.self)

      expectNotEmpty(unarchivedBoxNode)
      expectType(Optional<SCNNode>.self, &unarchivedBoxNode)

      expectEmpty(unarchivedBoxNode_nil)
    }
  }
}

runAllTests()

