// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NOUNWIND

// REQUIRES: swift_feature_CoroutineAccessors

var _i: Int = 1
public class C {
  public var i: Int {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }
}

// CHECK-LABEL: sil_vtable C {
// CHECK-NEXT:    #C.i!getter
// CHECK-NEXT:    #C.i!read2
// CHECK-NEXT:    #C.i!setter
// CHECK-NEXT:    #C.i!modify
// CHECK-NEXT:    #C.i!modify2
// CHECK-NEXT:    #C.init!allocator
// CHECK-NEXT:    #C.deinit!deallocator
// CHECK-NEXT:  }
