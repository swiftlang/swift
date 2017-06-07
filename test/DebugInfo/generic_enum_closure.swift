// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : T?
  init(x : __CurrentErrno) {
    // CHECK: define hidden {{.*}}void @_T020generic_enum_closure8CErrorOrVACyxGAA14__CurrentErrnoV1x_tcfC
    // CHECK-NOT: define
    // This is a SIL-level debug_value_addr instruction.
    // CHECK: call void @llvm.dbg.value({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    // CHECK: ![[T1:.*]] = !DICompositeType({{.*}}, identifier: "_T020generic_enum_closure8CErrorOrVyACQq_GD")
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", scope: {{.*}}, type: ![[T1]])
    value = .none
  }
  func isError() -> Bool {
    assert(value != nil, "the object should not contain an error")
    return false
  }
}
