// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

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
// CHECK: define {{.*}} @"$s4self11stuffStructVACycfC"(
// CHECK-NEXT: entry:
// CHECK: %[[ALLOCA:.*]] = alloca %T4self11stuffStructV, align {{(4|8)}}
// CHECK: #dbg_declare(ptr %[[ALLOCA]],
// CHECK-SAME: ![[SELF:.*]], !DIExpression(), !
// CHECK: ![[STUFFSTRUCT:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "stuffStruct", scope:{{.*}}identifier
// CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope
// CHECK-SAME:                         type: ![[STUFFSTRUCT]]

