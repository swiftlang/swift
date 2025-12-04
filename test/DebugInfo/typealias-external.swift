// RUN: %target-swift-frontend -emit-module-path %t/Alias.swiftmodule %S/Inputs/Alias.swift
// RUN: %target-swift-frontend %s -emit-ir -parse-as-library -module-name a -I%t -g -o - | %FileCheck %s
import Alias
public final class C {
  public func use(a: AliasFromModule) {}
}

let c = C()


// CHECK-DAG: !DILocalVariable(name: "a", arg: 1, {{.*}}, type: ![[CONST_TY:[0-9]+]])
// CHECK-DAG: ![[CONST_TY]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[EXIST_TY:[0-9]+]])
// CHECK-DAG: ![[EXIST_TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "ProtocolFromModule", {{.*}}, elements: ![[ELTS:[0-9]+]], runtimeLang: DW_LANG_Swift, identifier: "$s5Alias0A10FromModuleaD")
// CHECK-DAG: ![[ELTS]] = !{![[INNER:[0-9]+]]}
// CHECK-DAG: ![[INNER]] = !DIDerivedType(tag: DW_TAG_member, name: "$swift.constraint", {{.*}}, baseType: ![[TYPEDEF:[0-9]+]]
// CHECK-DAG: ![[TYPEDEF]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s5Alias0A10FromModuleaD", {{.*}}, baseType: ![[PROTO_TY:[0-9]+]])
// CHECK-DAG: ![[PROTO_TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "ProtocolFromModule", {{.*}}, identifier: "$s5Alias18ProtocolFromModule_pD")
