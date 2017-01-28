// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -primary-file %s -emit-ir -g -o - | %FileCheck %s

public struct stuffStruct {
    var a: Int64 = 6
    var b: String = "Nothing"
}

public func f() {
    var _: stuffStruct = stuffStruct()
}

// In the constructor, self has a type of "inout stuffStruct", but it
// is constructed in an alloca. The debug info for the alloca should not
// describe a reference type as we would normally do with inout arguments.
//
// CHECK: define {{.*}} @_T04self11stuffStructVACycfC(
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[ALLOCA:.*]] = alloca %V4self11stuffStruct, align {{(4|8)}}
// CHECK: call void @llvm.dbg.declare(metadata %V4self11stuffStruct* %[[ALLOCA]],
// CHECK-SAME: metadata ![[SELF:.*]], metadata !{{[0-9]+}}), !dbg
// CHECK: ![[STUFFSTRUCT:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "stuffStruct",{{.*}}identifier
// CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope
// CHECK-SAME:                         type: ![[STUFFSTRUCT]]

