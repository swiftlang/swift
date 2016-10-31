// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : T?
  init(x : __CurrentErrno) {
    // CHECK: define hidden void @_TFV20generic_enum_closure8CErrorOrCfT1xVS_14__CurrentErrno_GS0_x_
    // CHECK-NOT: define
    // This is a SIL-level debug_value_addr instruction.
    // CHECK: call void @llvm.dbg.value({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope
    // CHECK-SAME:                         type: ![[T1:.*]])
    // CHECK: ![[T1]] = !DICompositeType(
    // CHECK-SAME: identifier: "_TtGV20generic_enum_closure8CErrorOrQq_S0__"
    value = .none
  }
  func isError() -> Bool {
    assert(value != nil, "the object should not contain an error")
    return false
  }
}
