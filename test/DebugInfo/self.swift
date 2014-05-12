// RUN: %swift %s -emit-ir -g -o - | FileCheck %s
struct stuffStruct {
    var a: Int = 6
    var b: String = "Nothing"
}

func f() {
    var thing: stuffStruct = stuffStruct()
}

// In the constructor, self has a type of "inout stuffStruct", but it
// is constructed in an alloca. The debug info for the alloca should not
// describe a reference type as we would normally do with inout arguments.
//
// CHECK: define void  @_TFV4self11stuffStructCfMS0_FT_S0_(%V4self11stuffStruct* noalias sret) {
// CHECK-NEXT: entry:
// CHECK-NEXT: %[[ALLOCA:.*]] = alloca %V4self11stuffStruct, align 8
// CHECK:  call void @llvm.dbg.declare(metadata !{%V4self11stuffStruct* %[[ALLOCA]]}, metadata ![[SELF:.*]]), !dbg
// CHECK: null, null, metadata ![[STUFFSTRUCT:.*]]} ; [ DW_TAG_structure_type ] [stuffStruct]
// CHECK: ![[SELF]] = {{.*}}, metadata ![[STUFFSTRUCT]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [self]

