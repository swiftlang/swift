// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -primary-file %s -emit-ir -g -o - | %FileCheck %s

struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : T?
  init(x : __CurrentErrno) {
    // CHECK: define hidden {{.*}}void @_T020generic_enum_closure8CErrorOrVACyxGAA14__CurrentErrnoV1x_tcfC
    // CHECK-NOT: define
    // This is a SIL-level debug_value_addr instruction.
    // CHECK: call void @llvm.dbg.value({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope
    // CHECK-SAME:                         type: ![[T1:.*]])
    // CHECK: ![[T1]] = !DICompositeType(
    // CHECK-SAME: identifier: "_T020generic_enum_closure8CErrorOrVyACQq_GD"
    value = .none
  }
  func isError() -> Bool {
    assert(value != nil, "the object should not contain an error")
    return false
  }
}
