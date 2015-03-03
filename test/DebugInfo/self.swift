// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

public struct stuffStruct {
    var a: Int = 6
    var b: String = "Nothing"
}

public func f() {
    var thing: stuffStruct = stuffStruct()
}

// In the constructor, self has a type of "inout stuffStruct", but it
// is constructed in an alloca. The debug info for the alloca should not
// describe a reference type as we would normally do with inout arguments.
//
// CHECK: define {{.*}} @_TFV4self11stuffStructCfMS0_FT_S0_(
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[ALLOCA:.*]] = alloca %V4self11stuffStruct, align {{(4|8)}}
// CHECK: call void @llvm.dbg.declare(metadata %V4self11stuffStruct* %[[ALLOCA]], metadata ![[SELF:.*]], metadata !{{[0-9]+}}), !dbg
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "stuffStruct",
// CHECK-SAME:             identifier: [[STUFFSTRUCT:"[^"]+"]]
// CHECK: ![[SELF]] = !MDLocalVariable(tag: DW_TAG_auto_variable, name: "self",{{.*}} type: ![[STUFFSTRUCT]]

