// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
struct __CurrentErrno {}
struct CErrorOr<T>
 {
  var value : GenericIVar<Optional<T>>
  init(x : __CurrentErrno) {
    value = GenericIVar(.None)
  }
  func isError() -> Bool {
  // CHECK: _TtGSqQ__
    assert(value.value, "the object should not contain an error")
    return false
  }
}
