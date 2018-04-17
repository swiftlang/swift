// RUN: %target-swift-frontend -parse-stdlib %s -emit-ir -g -o - | %FileCheck %s
// CHECK:  !DIDerivedType(tag: DW_TAG_typedef, name: "$SBbD",
var bo : Builtin.BridgeObject
