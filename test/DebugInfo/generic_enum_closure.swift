// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : T?
  init(x : __CurrentErrno) {
    // CHECK: define hidden void @_TFV20generic_enum_closure8CErrorOrCU__fMGS0_Q__FT1xVS_14__CurrentErrno_GS0_Q__
    // CHECK-NOT: define
    // This is a SIL-level debug_value_addr instruction.
    // CHECK: call void @llvm.dbg.value({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    // CHECK-DAG: ![[SELF]] = !DILocalVariable(tag: DW_TAG_auto_variable, name: "self",{{.*}} type: !"_TtGV20generic_enum_closure8CErrorOrQq_S0__"
    value = .None
  }
  func isError() -> Bool {
    assert(value != nil, "the object should not contain an error")
    return false
  }
}
