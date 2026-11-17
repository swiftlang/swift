// RUN: %target-swift-frontend -emit-sil -Onone -sil-verify-all %s \
// RUN:   -enable-experimental-feature CoroutineAccessors \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-callee-allocated-coro-abi \
// RUN:   | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_Lifetimes

// Extending the scope of a nested coroutine over a yield must also keep its
// callee-allocated frame alive until the coroutine ends. Deallocating the
// frame before the yield let the outer coroutine's stack reuse the storage
// that the still-suspended inner coroutine was using.

public class Buf {
  private var _c = ContiguousArray<UInt8>(repeating: 0, count: 8)
  public init() {}

  // CHECK-LABEL: sil {{.*}}@$s{{.*}}3BufC8contentss11MutableSpanVys5UInt8VGvx :
  // CHECK:         ([[ADDR:%[0-9]+]], [[TOKEN:%[0-9]+]], [[FRAME:%[0-9]+]]) = begin_apply
  // CHECK-NOT:     dealloc_stack [[FRAME]]
  // CHECK:         yield
  // CHECK:       bb1:
  // CHECK:         end_apply [[TOKEN]]
  // CHECK:         dealloc_stack [[FRAME]]
  // CHECK:       bb2:
  // CHECK:         end_apply [[TOKEN]]
  // CHECK:         dealloc_stack [[FRAME]]
  public var contents: MutableSpan<UInt8> {
    yielding borrow { yield _c.mutableSpan }
    yielding mutate { var s = _c.mutableSpan; yield &s }
  }
}
