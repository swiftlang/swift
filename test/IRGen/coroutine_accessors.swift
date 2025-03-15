// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN: | %IRGenFileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors

@frozen
// CHECK-LABEL: %T19coroutine_accessors1SV = type <{ %AnyObject, %TSi }>
public struct S {
public var o: any AnyObject
public var _i: Int = 0

// CHECK-LABEL: @"$s19coroutine_accessors1SV3irmSivyTwc" = {{.*}}global %swift.coro_func_pointer
// CHECK-SAME:      <{ i32 trunc (
// CHECK-SAME:             i64 sub (
// CHECK-SAME:                 i64 ptrtoint (ptr @"$s19coroutine_accessors1SV3irmSivy" to i64),
// CHECK-SAME:                 i64 ptrtoint (ptr @"$s19coroutine_accessors1SV3irmSivyTwc" to i64)
// CHECK-SAME:             )
// CHECK-SAME:             to i32
// CHECK-SAME:         ),
// CHECK-SAME:         i32 0
// CHECK-SAME:      }>
// CHECK-LABEL: @"$s19coroutine_accessors1SV3irmSivxTwc" = {{.*}}global %swift.coro_func_pointer
// CHECK-SAME:      <{ i32 trunc (
// CHECK-SAME:             i64 sub (
// CHECK-SAME:                 i64 ptrtoint (ptr @"$s19coroutine_accessors1SV3irmSivx" to i64),
// CHECK-SAME:                 i64 ptrtoint (ptr @"$s19coroutine_accessors1SV3irmSivxTwc" to i64)
// CHECK-SAME:             )
// CHECK-SAME:             to i32
// CHECK-SAME:         ),
// CHECK-SAME:         i32 0
// CHECK-SAME:      }>

public var irm: Int {
// CHECK-LABEL: declare{{.*}} swiftcc void @"$s19coroutine_accessors1SVSiIetMIlYl_TC"(ptr noalias, ptr swiftcoro)

// CHECK-LABEL:     define{{.*}} { ptr, {{i64|i32}} } @"$s19coroutine_accessors1SV3irmSivy"(
// CHECK-SAME:          ptr noalias [[FRAME:%[^,]+]],
// CHECK-SAME:          ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:          ptr [[S_FIELD_O:%[^,]+]],
// CHECK-SAME:          i64 [[S_FIELD__I:%[^)]+]]
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:               [[ID:%[^,]+]] = call token @llvm.coro.id.retcon.once.dynamic(
// CHECK-SAME:                   i32 -1,
// CHECK-SAME:                   i32 16,
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SV3irmSivyTwc",
// CHECK-SAME:                   ptr [[ALLOCATOR]],
// CHECK-SAME:                   ptr [[FRAME]],
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SVSiIetMIgYy_TC",
// CHECK-SAME:                   ptr @swift_coro_alloc,
// CHECK-SAME:                   ptr @swift_coro_dealloc
// CHECK-SAME:               )
// CHECK:               [[HANDLE:%[^,]+]] = call ptr @llvm.coro.begin(
// CHECK-SAME:                   token [[ID]],
// CHECK-SAME:                   ptr null
// CHECK-SAME:               )
// CHECK:               call ptr (...) @llvm.coro.suspend.retcon.p0(i64 [[S_FIELD__I]])
// CHECK:               br i1 false, label %[[UNWIND:[^,]+]], label %[[NORMAL:[^,]+]]
// CHECK:             [[NORMAL]]:
// CHECK:               br label %coro.end
// CHECK:             [[UNWIND]]:
// CHECK:               br label %coro.end
// CHECK:             coro.end:
// CHECK:               call i1 @llvm.coro.end(
// CHECK-SAME:              ptr [[HANDLE]],
// CHECK-SAME:              i1 false,
// CHECK-SAME:              token none
// CHECK-SAME:          )
// CHECK:               unreachable
// CHECK:           }
  read {
    yield _i
  }
// CHECK-LABEL:     define{{.*}} { ptr, ptr } @"$s19coroutine_accessors1SV3irmSivx"(
// CHECK-SAME:          ptr noalias [[FRAME:%[^,]+]],
// CHECK-SAME:          ptr swiftcoro [[ALLOCATOR:%[^,]+]],
// CHECK-SAME:          ptr nocapture swiftself dereferenceable(16) [[SELF:%[^)]+]]
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:               [[ID:%[^,]+]] = call token @llvm.coro.id.retcon.once.dynamic(
// CHECK-SAME:                   i32 -1,
// CHECK-SAME:                   i32 16,
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SV3irmSivxTwc",
// CHECK-SAME:                   ptr [[ALLOCATOR]],
// CHECK-SAME:                   ptr [[FRAME]],
// CHECK-SAME:                   ptr @"$s19coroutine_accessors1SVSiIetMIlYl_TC",
// CHECK-SAME:                   ptr @swift_coro_alloc,
// CHECK-SAME:                   ptr @swift_coro_dealloc
// CHECK-SAME:               )
// CHECK:               [[HANDLE:%[^,]+]] = call ptr @llvm.coro.begin(
// CHECK-SAME:                   token [[ID]],
// CHECK-SAME:                   ptr null
// CHECK-SAME:               )
// CHECK:               [[S_FIELD__I:%[^,]+]] = getelementptr inbounds %T19coroutine_accessors1SV,
// CHECK-SAME:                     ptr [[SELF]],
// CHECK-SAME:                     i32 0,
// CHECK-SAME:                     i32 1
// CHECK:               call ptr (...) @llvm.coro.suspend.retcon.p0(
// CHECK-SAME:              ptr [[S_FIELD__I]]
// CHECK-SAME:          )
// CHECK:               br i1 false, label %[[UNWIND:[^,]+]], label %[[NORMAL:[^,]+]]
// CHECK:             [[NORMAL]]:
// CHECK:               br label %coro.end
// CHECK:             [[UNWIND]]:
// CHECK:               br label %coro.end
// CHECK:             coro.end:
// CHECK:               [[REGISTER_8:%[^,]+]] = call i1 @llvm.coro.end(ptr [[HANDLE]], i1 false, token none)
// CHECK:               unreachable
// CHECK:           }
  modify {
    yield &_i
  }
} // public var irm
}
