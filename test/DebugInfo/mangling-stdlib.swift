// RUN: %target-swift-frontend -parse-stdlib %s -emit-ir -g -o - | %FileCheck %s
// CHECK:  !DIDerivedType(tag: DW_TAG_typedef, name: "BridgeObject", {{.*}}baseType: ![[BT:[0-9]+]]
// CHECK: ![[BT]] = {{.*}}"$sBbD"
var bo : Builtin.BridgeObject
