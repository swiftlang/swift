// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

public protocol TheProtocol {}

public class TheClass: TheProtocol {
}

struct TheStruct<T> {
  let t: T
}

func f() -> some TheProtocol {
  let p: some TheProtocol = TheClass()
  return p
}

let v = f()
// CHECK: !DIGlobalVariable(name: "v", {{.*}}type: ![[FIXED_BUFFER:[0-9]+]]
// CHECK: ![[FIXED_BUFFER]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$swift.fixedbuffer", {{.*}}, elements: ![[TYPE_1:[0-9]+]]
// CHECK: ![[TYPE_1]] = !{![[TYPE_2:[0-9]+]]}
// CHECK: ![[TYPE_2]] = !DIDerivedType(tag: DW_TAG_member, name: "contents"{{.*}}baseType: ![[TYPE_3:[0-9]+]]
// CHECK: ![[TYPE_3]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TYPE:[0-9]+]])
// CHECK: ![[TYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "TheClass"


let v2 = TheStruct(t: f())
// CHECK: !DIGlobalVariable(name: "v2", {{.*}}type: ![[CONST_TYPE_GEN:[0-9]+]]
// CHECK: ![[CONST_TYPE_GEN]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[TYPE_GEN_1:[0-9]+]])
// CHECK: ![[TYPE_GEN_1]] = !DICompositeType(tag: DW_TAG_structure_type{{.*}}elements: ![[TYPE_GEN_2:[0-9]+]]
// CHECK: ![[TYPE_GEN_2]] = !{![[TYPE_GEN_3:[0-9]+]]}
// CHECK: ![[TYPE_GEN_3]] = !DIDerivedType(tag: DW_TAG_member{{.*}}baseType: ![[TYPE_GEN:[0-9]+]]
// CHECK: ![[TYPE_GEN]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s17global_opaque_var9TheStructVyAA0D5ClassCGD"
