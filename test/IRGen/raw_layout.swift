// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/raw_layout.sil
// RUN: %target-swift-frontend -enable-experimental-feature RawLayout -emit-ir %t/raw_layout.sil | %FileCheck %t/raw_layout.sil --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

import Swift

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}4LockVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 4
// stride
// CHECK-SAME:  , {{i64|i32}} 4
// flags: alignment 3, noncopyable
// CHECK-SAME:  , <i32 0x800003>

@_rawLayout(size: 4, alignment: 4)
struct Lock: ~Copyable { }

struct PaddedStride {
    var x: Int32
    var y: Int8
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}16LikePaddedStrideVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 5
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags: alignment 3, noncopyable
// CHECK-SAME:  , <i32 0x800003>
@_rawLayout(like: PaddedStride)
struct LikePaddedStride: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}22LikePaddedStrideArray1VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 8
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags: alignment 3, noncopyable
// CHECK-SAME:  , <i32 0x800003>
@_rawLayout(likeArrayOf: PaddedStride, count: 1)
struct LikePaddedStrideArray1: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}22LikePaddedStrideArray2VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 16
// stride
// CHECK-SAME:  , {{i64|i32}} 16
// flags: alignment 3, noncopyable, (on 32-bit platforms) not storable inline
// CHECK-64-SAME:  , <i32 0x800003>
// CHECK-32-SAME:  , <i32 0x820003>
@_rawLayout(likeArrayOf: PaddedStride, count: 2)
struct LikePaddedStrideArray2: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}9KeymasterVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 12
// stride
// CHECK-SAME:  , {{i64|i32}} 12
// flags: alignment 3, noncopyable
// CHECK-SAME:  , <i32 0x800003>
struct Keymaster: ~Copyable {
    let lock1: Lock
    let lock2: Lock
    let lock3: Lock
}

/*
TODO: Dependent layout not yet implemented

@_rawLayout(like: T)
struct Cell<T>: ~Copyable {}

@_rawLayout(likeArrayOf: T, count: 1)
struct PaddedCell<T>: ~Copyable {}

@_rawLayout(likeArrayOf: T, count: 8)
struct SmallVectorBuf<T>: ~Copyable {}
*/

sil @use_lock : $@convention(thin) (@in_guaranteed Lock) -> () {
entry(%L: $*Lock):
    return undef : $()
}

sil @use_keymaster_locks : $@convention(thin) (@in_guaranteed Keymaster) -> () {
entry(%K: $*Keymaster):
    %f = function_ref @use_lock : $@convention(thin) (@in_guaranteed Lock) -> ()
    %a = struct_element_addr %K : $*Keymaster, #Keymaster.lock1
    apply %f(%a) : $@convention(thin) (@in_guaranteed Lock) -> ()
    %b = struct_element_addr %K : $*Keymaster, #Keymaster.lock2
    apply %f(%b) : $@convention(thin) (@in_guaranteed Lock) -> ()
    %c = struct_element_addr %K : $*Keymaster, #Keymaster.lock2
    apply %f(%c) : $@convention(thin) (@in_guaranteed Lock) -> ()
    return undef : $()
}
