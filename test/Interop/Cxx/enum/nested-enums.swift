// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)

// REQUIRES: executable_test

import NestedEnums
import StdlibUnittest

var NestedEnumsTestSuite = TestSuite("Nested Enums")

// NestedEnumsTestSuite.test("Make and compare") {
//   let val: ns.EnumInNS = ns.kA
//   expectNotEqual(val, ns.kB)
//   let valNested = ns.nestedNS.kNestedA
//   expectNotEqual(valNested, ns.nestedNS.kNestedB)
// }

// extension HdCamera.Projection {
//   @_documentation(visibility: internal) public static var Perspective: HdCamera.Projection { HdCamera.Projection.Perspective }
//   @_documentation(visibility: internal) public static var Orthographic: HdCamera.Projection { HdCamera.Projection.Orthographic }
// }

NestedEnumsTestSuite.test("HdCamera.Orthographic") {
  let x : HdCamera.Projection = HdCamera.Orthographic
  let y : HdCamera.Projection = HdCamera.Orthographic
  let xx : GfCamera.Projection = GfCamera.Orthographic
  let yy : GfCamera.Projection = GfCamera.Orthographic

  print(xx == yy)
  print(x == y)
  // print("First check")
  // expectEqual(xx, yy)
  
  // print("Second check")
  // expectEqual(x, y)
  // print("End")
}

runAllTests()
