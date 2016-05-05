// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

public struct stuffStruct {
    var a: Int64 = 6
    var b: String = "Nothing"
}

public func f() {
    var thing: stuffStruct = stuffStruct()
}

// In the constructor, self has a type of "inout stuffStruct", but it
// is constructed in an alloca. The debug info for the alloca should not
// describe a reference type as we would normally do with inout arguments.

// CHECK: define {{.*}} @_TFV4self11stuffStructCfT_S0_(
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[ALLOCA:.*]] = alloca %V4self11stuffStruct, align {{(4|8)}}
// CHECK: call void @llvm.dbg.declare(metadata %V4self11stuffStruct* %[[ALLOCA]], metadata ![[SELF:[0-9]+]], metadata !{{[0-9]+}}), !dbg

// CHECK-DAG: ![[SELF]] = !DILocalVariable(name: "self", arg: 1, {{.*}}, type: ![[STUFFSTRUCT:[0-9]+]]
// CHECK-DAG: ![[STUFFSTRUCT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "stuffStruct", {{.*}}, identifier: "_TtV4self11stuffStruct"

