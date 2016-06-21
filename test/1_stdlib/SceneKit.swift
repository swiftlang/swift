// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest


import SceneKit

// SceneKit is only available on iOS 8.0 and above and on OS X 10.8 and above.
// That said SCNMatrix4Scale &co are only available on OS X 10.10 and above.

var SceneKitTests = TestSuite("SceneKit")

if #available(iOS 8.0, OSX 10.10, *) {
  let scn_vec3_ref = SCNVector3Make(1, 2, 3)
  let scn_vec4_ref = SCNVector4Make(1, 2, 3, 4)
  let scn_mat4_ref = SCNMatrix4Scale(SCNMatrix4Translate(SCNMatrix4MakeRotation(SCNFloat(M_PI), 1, 0, 0), 1, 2, 3), 10, 20, 30)

  // MARK: Exposing SCNFloat

  SceneKitTests.test("SCNFloat") {
    let f = Float(1.0)
    let scn_float_from_f = SCNFloat(f)
    expectEqual(scn_float_from_f, 1.0)

    let d = Double(2.0)
    let scn_float_from_d = SCNFloat(d)
    expectEqual(scn_float_from_d, 2.0)

    let cg = CGFloat(3.0)
    let scn_float_from_cg = SCNFloat(cg as NSNumber)
    expectEqual(scn_float_from_cg, 3.0)

    let node = SCNNode()
    node.position.x = scn_float_from_f
    node.position.y = scn_float_from_d
    node.position.z = scn_float_from_cg
    expectTrue(SCNVector3EqualToVector3(node.position, scn_vec3_ref))

    let f1: SCNFloat = scn_vec3_ref.x
    let f2: SCNFloat = scn_vec4_ref.y
    expectEqual(f1, 1.0);
    expectEqual(f2, 2.0);
  }

  // MARK: Working with SCNVector3

  SceneKitTests.test("SCNVector3.init()/Literal") {
    let scn_vec3_from_lit = SCNVector3(1, 2, 3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_lit, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/Float") {
    let f1: Float = 1.0
    let f2: Float = 2.0
    let f3: Float = 3.0
    let scn_vec3_from_f = SCNVector3(f1, f2, f3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_f, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/CGFloat") {
    let d1: CGFloat = 1.0
    let d2: CGFloat = 2.0
    let d3: CGFloat = 3.0
    let scn_vec3_from_cg = SCNVector3(d1, d2, d3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_cg, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/Double") {
    let d1: Double = 1.0
    let d2: Double = 2.0
    let d3: Double = 3.0
    let scn_vec3_from_d = SCNVector3(d1, d2, d3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_d, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/Int") {
    let i1: Int = 1
    let i2: Int = 2
    let i3: Int = 3
    let scn_vec3_from_i = SCNVector3(i1, i2, i3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_i, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/float3") {
    let v3 = float3(1, 2, 3)
    let scn_vec3_from_float3 = SCNVector3(v3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_float3, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/double3") {
    let v3 = double3(1, 2, 3)
    let scn_vec3_from_double3 = SCNVector3(v3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_double3, scn_vec3_ref))
  }

  SceneKitTests.test("SCNVector3.init()/mix") {
    let f1: Float = 1.0
    let d2: CGFloat = 2.0
    let scn_vec3_from_mixed = SCNVector3(f1, Float(d2), 3)
    expectTrue(SCNVector3EqualToVector3(scn_vec3_from_mixed, scn_vec3_ref))
  }

  SceneKitTests.test("float3.init()/SCNVector3") {
    let v3 = float3(scn_vec3_ref)
    expectEqual(v3.x, 1.0)
    expectEqual(v3.y, 2.0)
    expectEqual(v3.z, 3.0)
  }

  SceneKitTests.test("double3.init()/SCNVector3") {
    let v3 = double3(scn_vec3_ref)
    expectEqual(v3.x, 1.0)
    expectEqual(v3.y, 2.0)
    expectEqual(v3.z, 3.0)
  }

  // MARK: Working with SCNVector4

  SceneKitTests.test("SCNVector4.init()/Literal") {
    let scn_vec4_from_lit = SCNVector4(1, 2, 3, 4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_lit, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/Float") {
    let f1: Float = 1.0
    let f2: Float = 2.0
    let f3: Float = 3.0
    let f4: Float = 4.0
    let scn_vec4_from_f = SCNVector4(f1, f2, f3, f4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_f, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/CGFloat") {
    let d1: CGFloat = 1.0
    let d2: CGFloat = 2.0
    let d3: CGFloat = 3.0
    let d4: CGFloat = 4.0
    let scn_vec4_from_cg = SCNVector4(d1, d2, d3, d4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_cg, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/Double") {
    let d1: Double = 1.0
    let d2: Double = 2.0
    let d3: Double = 3.0
    let d4: Double = 4.0
    let scn_vec4_from_d = SCNVector4(d1, d2, d3, d4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_d, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/Int") {
    let i1: Int = 1
    let i2: Int = 2
    let i3: Int = 3
    let i4: Int = 4
    let scn_vec4_from_i = SCNVector4(i1, i2, i3, i4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_i, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/float4") {
    let v4 = float4(1, 2, 3, 4)
    let scn_vec4_from_float4 = SCNVector4(v4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_float4, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/double4") {
    let v4 = double4(1, 2, 3, 4)
    let scn_vec4_from_double4 = SCNVector4(v4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_double4, scn_vec4_ref))
  }

  SceneKitTests.test("SCNVector4.init()/mix") {
    let f1: Float = 1.0
    let d2: CGFloat = 2.0
    let f4: Float = 4.0
    let scn_vec4_from_mixed = SCNVector4(f1, Float(d2), 3, f4)
    expectTrue(SCNVector4EqualToVector4(scn_vec4_from_mixed, scn_vec4_ref))
  }

  SceneKitTests.test("float4.init()/SCNVector4") {
    let v4 = float4(scn_vec4_ref)
    expectEqual(v4.x, 1.0)
    expectEqual(v4.y, 2.0)
    expectEqual(v4.z, 3.0)
    expectEqual(v4.w, 4.0)
  }

  SceneKitTests.test("double4.init()/SCNVector4") {
    let v4 = double4(scn_vec4_ref)
    expectEqual(v4.x, 1.0)
    expectEqual(v4.y, 2.0)
    expectEqual(v4.z, 3.0)
    expectEqual(v4.w, 4.0)
  }

  // MARK: Working with SCNMatrix4

  SceneKitTests.test("SCNMatrix4.init()/float4x4 + float4x4.init()/SCNMatrix4") {
    let mat4_from_scn_mat4 = float4x4(scn_mat4_ref)
    let scn_vec4_from_mat4 = SCNMatrix4(mat4_from_scn_mat4)
    expectTrue(SCNMatrix4EqualToMatrix4(scn_vec4_from_mat4, scn_mat4_ref))
  }

  SceneKitTests.test("SCNMatrix4.init()/double4x4 + double4x4.init()/SCNMatrix4") {
    let mat4_from_scn_mat4 = double4x4(scn_mat4_ref)
    let scn_vec4_from_mat4 = SCNMatrix4(mat4_from_scn_mat4)
    expectTrue(SCNMatrix4EqualToMatrix4(scn_vec4_from_mat4, scn_mat4_ref))
  }
}

runAllTests()

