// RUN: %target-swift-frontend -emit-ir -g %s -o - -parse-stdlib | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK: !DICompositeType(tag: DW_TAG_array_type, baseType: ![[FLOAT:[0-9]+]], size: 64, flags: DIFlagVector, elements: ![[ELTS:[0-9]+]])
// CHECK: ![[FLOAT]] = !DIBasicType(name: "$sBf32_D", size: 32, encoding: DW_ATE_float)
// CHECK: ![[ELTS]] = !{![[SR:[0-9]+]]}
// CHECK: ![[SR]] = !DISubrange(count: 2, lowerBound: 0)


import Swift
public func float2(_ _vector: Builtin.Vec2xFPIEEE32, _ index: UInt) {
  let elt = Builtin.extractelement_Vec2xFPIEEE32_Int32(_vector,
    Int32(index)._value)
}
