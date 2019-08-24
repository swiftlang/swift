// RUN: %target-swift-frontend -emit-ir -disable-llvm-optzns -primary-file %s | %FileCheck %s
extension Dictionary {
  subscript(alternate key: Key) -> Value? {
    get {
      return self[key]
    }
    _modify {
      var value: Value? = nil
      yield &value
    }
  }
}

// CHECK-LABEL: define {{.*}}sSD14modifyaccessorE9alternateq_Sgx_tciM
// CHECK: [[COROALLOCA:%.*]] = call token @llvm.coro.alloca.alloc
// CHECK: call void @llvm.coro.alloca.free(token [[COROALLOCA]])
