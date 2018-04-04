// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : T?
  init(x : __CurrentErrno) {
    // CHECK: define hidden {{.*}}void @"$S20generic_enum_closure8CErrorOrV1xACyxGAA14__CurrentErrnoV_tcfC"
    // CHECK-NOT: define
    // This is a SIL-level debug_value_addr instruction.
    // CHECK: call void @llvm.dbg.declare
    // Self is in a dynamic alloca, hence the shadow copy.
    // CHECK: call void @llvm.dbg.declare(
    // CHECK-SAME: metadata i8** %[[SHADOW:.*]], metadata ![[SELF:.*]], meta
    // CHECK-SAME: !DIExpression(DW_OP_deref))
    // CHECK-DAG: store i8* %[[DYN:.*]], i8** %[[SHADOW]]
    // CHECK-DAG: %[[DYN]] = alloca i8, i{{32|64}} %
    // CHECK: ![[T1:.*]] = !DICompositeType({{.*}}, identifier: "$S20generic_enum_closure8CErrorOrVyACQq_GD")
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope:
    // CHECK-SAME:                         type: ![[T1]])
    value = .none
  }
}
