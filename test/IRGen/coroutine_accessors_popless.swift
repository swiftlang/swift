// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN:     -enable-arm64-corocc                                            \
// RUN:     -enable-x86_64-corocc                                           \
// RUN: | %IRGenFileCheck %s

// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=x86_64
// REQUIRES: swift_feature_CoroutineAccessors

// CHECK-LABEL: @__swift_coro_alloc_(
// CHECK-SAME:      ptr [[ALLOCATOR:%[^,]+]]
// CHECK-SAME:      i64 [[SIZE:%[^)]+]]
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       entry:
// CHECK:         [[USE_POPLESS:%[^,]+]] = icmp eq ptr [[ALLOCATOR]], null
// CHECK:         br i1 [[USE_POPLESS]], 
// CHECK-SAME:        label %coro.return.popless
// CHECK-SAME:        label %coro.return.normal
// CHECK:       coro.return.popless:
// CHECK:         [[STACK_ALLOCATION:%[^,]+]] = alloca i8, i64 [[SIZE]], align 16
// CHECK:         musttail call void @llvm.ret.popless()
// CHECK:         ret ptr [[STACK_ALLOCATION]]
// CHECK:       coro.return.normal:
// CHECK:         [[OTHER_ALLOCATION:%[^,]+]] = call swiftcc ptr @swift_coro_alloc(
// CHECK-SAME:        ptr [[ALLOCATOR]]
// CHECK-SAME:        i64 [[SIZE]]
// CHECK-SAME:    )
// CHECK:         ret ptr [[OTHER_ALLOCATION]]
// CHECK:       }

public var _i: Int = 0

public var i: Int {
  read {
    yield _i
  }
}

