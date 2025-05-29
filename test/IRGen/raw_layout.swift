// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/raw_layout.sil
// RUN: %target-swift-frontend -enable-experimental-feature RawLayout -emit-ir -disable-availability-checking -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %t/raw_layout.sil | %FileCheck %t/raw_layout.sil --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: swift_feature_RawLayout

import Builtin
import Swift
import RawLayoutCXX

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}4LockVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 4
// stride
// CHECK-SAME:  , {{i64|i32}} 4
// flags: alignment 3, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>

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
// flags: alignment 3, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
@_rawLayout(like: PaddedStride)
struct LikePaddedStride: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}22LikePaddedStrideArray1VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 8
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags: alignment 3, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
@_rawLayout(likeArrayOf: PaddedStride, count: 1)
struct LikePaddedStrideArray1: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}22LikePaddedStrideArray2VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 16
// stride
// CHECK-SAME:  , {{i64|i32}} 16
// flags: alignment 3, noncopyable, non-bitwise-borrowable, (on 32-bit platforms) not storable inline
// CHECK-64-SAME:  , <i32 0x1800003>
// CHECK-32-SAME:  , <i32 0x1820003>
@_rawLayout(likeArrayOf: PaddedStride, count: 2)
struct LikePaddedStrideArray2: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}9KeymasterVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 12
// stride
// CHECK-SAME:  , {{i64|i32}} 12
// flags: alignment 3, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
struct Keymaster: ~Copyable {
    let lock1: Lock
    let lock2: Lock
    let lock3: Lock
}

// Dependent Layouts:
// These types have their layouts all zeroed out and will be fixed up at
// runtime.

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}4CellVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(like: T)
struct Cell<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}10PaddedCellVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: 1)
struct PaddedCell<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}14SmallVectorBufVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: 8)
struct SmallVectorBuf<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}14SmallVectorOf3VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: 3)
struct SmallVectorOf3<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}6VectorVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: N)
struct Vector<T, let N: Int>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}8UsesCellVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 8
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags: alignment 3, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
struct UsesCell: ~Copyable {
    let someCondition: Bool
    let specialInt: Cell<Int32>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}13BufferOf3BoolVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 3
// stride
// CHECK-SAME:  , {{i64|i32}} 3
// flags: alignment 0, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800000>
struct BufferOf3Bool: ~Copyable {
    let buffer: SmallVectorOf3<Bool>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}9BadBufferVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 48
// stride
// CHECK-SAME:  , {{i64|i32}} 48
// flags: alignment 7, noncopyable, non-bitwise-borrowable, is not inline
// CHECK-SAME:  , <i32 0x1820007>
struct BadBuffer: ~Copyable {
    let buffer: SmallVectorOf3<Int64?>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}10UsesVectorVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 2
// stride
// CHECK-SAME:  , {{i64|i32}} 2
// flags: alignment 0, noncopyable, non-bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800000>
struct UsesVector: ~Copyable {
    let buffer: Vector<UInt8, 2>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}10BadBuffer2VWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 48
// stride
// CHECK-SAME:  , {{i64|i32}} 48
// flags: alignment 7, noncopyable, non-bitwise-borrowable, is not inline
// CHECK-SAME:  , <i32 0x1820007>
struct BadBuffer2: ~Copyable {
    let buffer: Vector<Int64?, 3>
}

// Raw Layout types that move like their like type

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}19CellThatMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout19CellThatMovesAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout19CellThatMovesAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout19CellThatMovesAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(like: T, movesAsLike)
struct CellThatMovesAsLike<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}18ConcreteMoveAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout18ConcreteMoveAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout18ConcreteMoveAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout18ConcreteMoveAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 1
// stride
// CHECK-SAME:  , {{i64|i32}} 1
// flags: not copyable, not bitwise takable, not pod, not inline
// CHECK-SAME:  , <i32 0x930000>
struct ConcreteMoveAsLike: ~Copyable {
  let cell: CellThatMovesAsLike<NonBitwiseTakableCXXType>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}21ConcreteIntMoveAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @__swift_noop_void_return
// initializeWithTake
// CHECK-SAME:  , ptr @__swift_memcpy4_4
// assignWithTake
// CHECK-SAME:  , ptr @__swift_memcpy4_4
// size
// CHECK-SAME:  , {{i64|i32}} 4
// stride
// CHECK-SAME:  , {{i64|i32}} 4
// flags: alignment 3, not copyable, not bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
struct ConcreteIntMoveAsLike: ~Copyable {
  let cell: CellThatMovesAsLike<Int32>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}25SmallVectorOf2MovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: 2, movesAsLike)
struct SmallVectorOf2MovesAsLike<T: ~Copyable>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}30ConcreteSmallVectorMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 2
// stride
// CHECK-SAME:  , {{i64|i32}} 2
// flags: not copyable, not bitwise takable, not pod, not inline
// CHECK-SAME:  , <i32 0x930000>
struct ConcreteSmallVectorMovesAsLike: ~Copyable {
  let vector: SmallVectorOf2MovesAsLike<NonBitwiseTakableCXXType>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}33ConcreteSmallVectorIntMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @__swift_noop_void_return
// initializeWithTake
// CHECK-SAME:  , ptr @__swift_memcpy8_4
// assignWithTake
// CHECK-SAME:  , ptr @__swift_memcpy8_4
// size
// CHECK-SAME:  , {{i64|i32}} 8
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags: alignment 3, not copyable, not bitwise-borrowable
// CHECK-SAME:  , <i32 0x1800003>
struct ConcreteSmallVectorIntMovesAsLike: ~Copyable {
  let vector: SmallVectorOf2MovesAsLike<Int32>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}17VectorMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout17VectorMovesAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout17VectorMovesAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout17VectorMovesAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 0
// stride
// CHECK-SAME:  , {{i64|i32}} 0
// flags: alignment 0, incomplete
// CHECK-SAME:  , <i32 0x400000>
@_rawLayout(likeArrayOf: T, count: N, movesAsLike)
struct VectorMovesAsLike<T: ~Copyable, let N: Int>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}25ConcreteVectorMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @"$s10raw_layout25ConcreteVectorMovesAsLikeVwxx
// initializeWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout25ConcreteVectorMovesAsLikeVwtk
// assignWithTake
// CHECK-SAME:  , ptr @"$s10raw_layout25ConcreteVectorMovesAsLikeVwta
// size
// CHECK-SAME:  , {{i64|i32}} 4
// stride
// CHECK-SAME:  , {{i64|i32}} 4
// flags: not copyable, not bitwise takable, not pod, not inline
// CHECK-SAME:  , <i32 0x930000>
struct ConcreteVectorMovesAsLike: ~Copyable {
  let vector: VectorMovesAsLike<NonBitwiseTakableCXXType, 4>
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}28ConcreteVectorIntMovesAsLikeVWV" = {{.*}} %swift.vwtable
// destroy
// CHECK-SAME:  , ptr @__swift_noop_void_return
// initializeWithTake
// CHECK-SAME:  , ptr @__swift_memcpy16_4
// assignWithTake
// CHECK-SAME:  , ptr @__swift_memcpy16_4
// size
// CHECK-SAME:  , {{i64|i32}} 16
// stride
// CHECK-SAME:  , {{i64|i32}} 16
// flags: alignment 3, not copyable, not bitwise-borrowable, (on 32-bit platforms) not storable inline
// CHECK-64-SAME:  , <i32 0x1800003>
// CHECK-32-SAME:  , <i32 0x1820003>
struct ConcreteVectorIntMovesAsLike: ~Copyable {
  let vector: VectorMovesAsLike<Int32, 4>
}

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

// CHECK: define {{.*}}swiftcc ptr @get_cell_addr(ptr %"Cell<T>", ptr {{.*}} swiftself{{.*}} [[SELF:%.*]])
// CHECK-NEXT:   entry:
// CHECK-NEXT:     ret ptr [[SELF]]
sil @get_cell_addr : $@convention(method) <T> (@in_guaranteed Cell<T>) -> UnsafeMutablePointer<T> {
entry(%0 : $*Cell<T>):
    %1 = builtin "addressOfRawLayout"(%0 : $*Cell<T>) : $Builtin.RawPointer
    %2 = struct $UnsafeMutablePointer<T> (%1 : $Builtin.RawPointer)
    return %2 : $UnsafeMutablePointer<T>
}

//===----------------------------------------------------------------------===//
// Dependent layout metadata initialization
//===----------------------------------------------------------------------===//

// Cell<T>

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}4CellVMr"(ptr %"Cell<T>", ptr {{.*}}, ptr {{.*}})
// CHECK: [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"Cell<T>", {{i64|i32}} 2
// CHECK-NEXT: [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK-NEXT: [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_checkMetadataState({{i64|i32}} 319, ptr [[T]])
// CHECK-NEXT: [[T:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK: [[T_VWT_ADDR:%.*]] = getelementptr inbounds ptr, ptr [[T]], {{i64|i32}} -1
// CHECK-NEXT: {{%.*}} = load ptr, ptr [[T_VWT_ADDR]]
// CHECK: [[T_LAYOUT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i32 8
// CHECK-NEXT: call void @swift_initRawStructMetadata2(ptr %"Cell<T>", {{i64|i32}} 0, ptr [[T_LAYOUT]], {{i64|i32}} 0, {{i64|i32}} 0)

// PaddedCell<T>

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}10PaddedCellVMr"(ptr %"PaddedCell<T>", ptr {{.*}}, ptr {{.*}})
// CHECK: call void @swift_initRawStructMetadata2(ptr %"PaddedCell<T>", {{i64|i32}} 0, ptr {{%.*}}, {{i64|i32}} 1, {{i64|i32}} 1)

// SmallVectorBuf<T>

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}14SmallVectorBufVMr"(ptr %"SmallVectorBuf<T>", ptr {{.*}}, ptr {{.*}})
// CHECK: call void @swift_initRawStructMetadata2(ptr %"SmallVectorBuf<T>", {{i64|i32}} 0, ptr {{%.*}}, {{i64|i32}} 8, {{i64|i32}} 1)

// Vector<T, N>

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}6VectorVMr"(ptr %"Vector<T, N>", ptr {{.*}}, ptr {{.*}})
// CHECK:         [[N_GEP:%.*]] = getelementptr inbounds {{i64|i32}}, ptr %"Vector<T, N>", {{i64|i32}} 3
// CHECK-NEXT:    [[N:%.*]] = load {{i64|i32}}, ptr [[N_GEP]]
// CHECK:         call void @swift_initRawStructMetadata2(ptr %"Vector<T, N>", {{i64|i32}} 0, ptr {{%.*}}, {{i64|i32}} [[N]], {{i64|i32}} 1)

//===----------------------------------------------------------------------===//
// CellThatMovesAsLike<T> Dependent layout metadata initialization
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}19CellThatMovesAsLikeVMr"(ptr %"CellThatMovesAsLike<T>", ptr {{.*}}, ptr {{.*}})
// CHECK: call void @swift_initRawStructMetadata2(ptr %"CellThatMovesAsLike<T>", {{i64|i32}} 0, ptr {{%.*}}, {{i64|i32}} 0, {{i64|i32}} 2)

//===----------------------------------------------------------------------===//
// CellThatMovesAsLike<T> destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout19CellThatMovesAsLikeVwxx"(ptr {{.*}} %object, ptr %"CellThatMovesAsLike<T>")
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"CellThatMovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         call void %Destroy(ptr {{.*}} %object, ptr %T)

//===----------------------------------------------------------------------===//
// CellThatMovesAsLike<T> initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout19CellThatMovesAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"CellThatMovesAsLike<T>")
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"CellThatMovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         {{%.*}} = call ptr %InitializeWithTake(ptr {{.*}} %dest, ptr {{.*}} %src, ptr [[T]])

//===----------------------------------------------------------------------===//
// CellThatMovesAsLike<T> assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout19CellThatMovesAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"CellThatMovesAsLike<T>")
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"CellThatMovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         {{%.*}} = call ptr %AssignWithTake(ptr {{.*}} %dest, ptr {{.*}} %src, ptr [[T]])

//===----------------------------------------------------------------------===//
// ConcreteMoveAsLike destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout18ConcreteMoveAsLikeVwxx"(ptr {{.*}} %object, ptr %ConcreteMoveAsLike)
// CHECK:         [[OBJ_CELL:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout18ConcreteMoveAsLikeV, ptr %object, i32 0, i32 0
// CHECK:         {{invoke void|invoke ptr|call void|call ptr}} @{{.*}}(ptr [[OBJ_CELL]])

//===----------------------------------------------------------------------===//
// ConcreteMoveAsLike initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout18ConcreteMoveAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteMoveAsLike)
// CHECK:         [[DEST_CELL:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout18ConcreteMoveAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_CELL:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout18ConcreteMoveAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_CELL]], ptr [[SRC_CELL]])

//===----------------------------------------------------------------------===//
// ConcreteMoveAsLike assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout18ConcreteMoveAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteMoveAsLike)
// CHECK:         [[DEST_CELL:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout18ConcreteMoveAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_CELL:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout18ConcreteMoveAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_CELL]], ptr [[SRC_CELL]])

//===----------------------------------------------------------------------===//
// SmallVectorOf2MovesAsLike<T> destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwxx"(ptr {{.*}} %object, ptr %"SmallVectorOf2MovesAsLike<T>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"SmallVectorOf2MovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[OBJ_ELT:%.*]] = getelementptr inbounds i8, ptr %object, {{i64|i32}} [[OFFSET]]
// CHECK:         call void %Destroy(ptr {{.*}} [[OBJ_ELT]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret void

//===----------------------------------------------------------------------===//
// SmallVectorOf2MovesAsLike<T> initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"SmallVectorOf2MovesAsLike<T>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"SmallVectorOf2MovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET_0:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[SRC_ELT_0:%.*]] = getelementptr inbounds i8, ptr %src, {{i64|i32}} [[OFFSET_0]]
// CHECK-NEXT:    [[DEST_ELT_0:%.*]] = getelementptr inbounds i8, ptr %dest, {{i64|i32}} [[OFFSET_0]]
// CHECK:         {{%.*}} = call ptr %InitializeWithTake(ptr {{.*}} [[DEST_ELT_0]], ptr {{.*}} [[SRC_ELT_0]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// SmallVectorOf2MovesAsLike<T> assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout25SmallVectorOf2MovesAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"SmallVectorOf2MovesAsLike<T>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"SmallVectorOf2MovesAsLike<T>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET_0:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[SRC_ELT_0:%.*]] = getelementptr inbounds i8, ptr %src, {{i64|i32}} [[OFFSET_0]]
// CHECK-NEXT:    [[DEST_ELT_0:%.*]] = getelementptr inbounds i8, ptr %dest, {{i64|i32}} [[OFFSET_0]]
// CHECK:         {{%.*}} = call ptr %AssignWithTake(ptr {{.*}} [[DEST_ELT_0]], ptr {{.*}} [[SRC_ELT_0]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// ConcreteSmallVectorMovesAsLike destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwxx"(ptr {{.*}} %object, ptr %ConcreteSmallVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[OBJ_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout30ConcreteSmallVectorMovesAsLikeV, ptr %object, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[OBJECT:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[OBJ_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    {{invoke void|invoke ptr|call void|call ptr}} @{{.*}}(ptr [[OBJECT]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret void

//===----------------------------------------------------------------------===//
// ConcreteSmallVectorMovesAsLike initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteSmallVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[DEST_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout30ConcreteSmallVectorMovesAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout30ConcreteSmallVectorMovesAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[SRC_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[SRC_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    [[DEST_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[DEST_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_0]], ptr [[SRC_0]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// ConcreteSmallVectorMovesAsLike assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout30ConcreteSmallVectorMovesAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteSmallVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[DEST_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout30ConcreteSmallVectorMovesAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout30ConcreteSmallVectorMovesAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[SRC_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[SRC_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    [[DEST_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[DEST_VECTOR]], {{i64|i32}} [[I]]
// CHECK:         {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_0]], ptr [[SRC_0]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 2
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// VectorMovesAsLike Dependent layout metadata initialization
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} swiftcc %swift.metadata_response @"$s{{[A-Za-z0-9_]*}}17VectorMovesAsLikeVMr"(ptr %"VectorMovesAsLike<T, N>", ptr {{.*}}, ptr {{.*}})
// CHECK:         [[N_GEP:%.*]] = getelementptr inbounds {{i64|i32}}, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 3
// CHECK-NEXT:    [[N:%.*]] = load {{i64|i32}}, ptr [[N_GEP]]
// CHECK:         call void @swift_initRawStructMetadata2(ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 0, ptr {{%.*}}, {{i64|i32}} [[N]], {{i64|i32}} 3)

//===----------------------------------------------------------------------===//
// VectorMovesAsLike destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout17VectorMovesAsLikeVwxx"(ptr {{.*}} %object, ptr %"VectorMovesAsLike<T, N>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[N_GEP:%.*]] = getelementptr inbounds {{i64|i32}}, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 3
// CHECK-NEXT:    [[N:%.*]] = load {{i64|i32}}, ptr [[N_GEP]]
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         [[COND:%.*]] = icmp sgt {{i64|i32}} [[N]], 0
// CHECK:         br i1 [[COND]], label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[OBJECT:%.*]] = getelementptr inbounds i8, ptr %object, {{i64|i32}} [[OFFSET]]
// CHECK:         call void %Destroy(ptr {{.*}} [[OBJECT]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], [[N]]
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret void

//===----------------------------------------------------------------------===//
// VectorMovesAsLike initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout17VectorMovesAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"VectorMovesAsLike<T, N>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[N_GEP:%.*]] = getelementptr inbounds {{i64|i32}}, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 3
// CHECK-NEXT:    [[N:%.*]] = load {{i64|i32}}, ptr [[N_GEP]]
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         [[COND:%.*]] = icmp sgt {{i64|i32}} [[N]], 0
// CHECK:         br i1 [[COND]], label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET_0:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[SRC_ELT_0:%.*]] = getelementptr inbounds i8, ptr %src, {{i64|i32}} [[OFFSET_0]]
// CHECK-NEXT:    [[DEST_ELT_0:%.*]] = getelementptr inbounds i8, ptr %dest, {{i64|i32}} [[OFFSET_0]]
// CHECK:         {{%.*}} = call ptr %InitializeWithTake(ptr {{.*}} [[DEST_ELT_0]], ptr {{.*}} [[SRC_ELT_0]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], [[N]]
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// VectorMovesAsLike assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout17VectorMovesAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %"VectorMovesAsLike<T, N>")
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[N_GEP:%.*]] = getelementptr inbounds {{i64|i32}}, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 3
// CHECK-NEXT:    [[N:%.*]] = load {{i64|i32}}, ptr [[N_GEP]]
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         [[COND:%.*]] = icmp sgt {{i64|i32}} [[N]], 0
// CHECK:         br i1 [[COND]], label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[T_ADDR:%.*]] = getelementptr inbounds ptr, ptr %"VectorMovesAsLike<T, N>", {{i64|i32}} 2
// CHECK-NEXT:    [[T:%.*]] = load ptr, ptr [[T_ADDR]]
// CHECK:         [[STRIDE_GEP:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr {{%.*}}, i32 0, i32 9
// CHECK-NEXT:    [[STRIDE:%.*]] = load {{i64|i32}}, ptr [[STRIDE_GEP]]
// CHECK:         [[OFFSET_0:%.*]] = mul {{i64|i32}} [[I]], [[STRIDE]]
// CHECK-NEXT:    [[SRC_ELT_0:%.*]] = getelementptr inbounds i8, ptr %src, {{i64|i32}} [[OFFSET_0]]
// CHECK-NEXT:    [[DEST_ELT_0:%.*]] = getelementptr inbounds i8, ptr %dest, {{i64|i32}} [[OFFSET_0]]
// CHECK:         {{%.*}} = call ptr %AssignWithTake(ptr {{.*}} [[DEST_ELT_0]], ptr {{.*}} [[SRC_ELT_0]], ptr [[T]])
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], [[N]]
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// ConcreteVectorMovesAsLike destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} void @"$s10raw_layout25ConcreteVectorMovesAsLikeVwxx"(ptr {{.*}} %object, ptr %ConcreteVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[OBJ_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout25ConcreteVectorMovesAsLikeV, ptr %object, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[OBJECT:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[OBJ_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    {{invoke void|invoke ptr|call void|call ptr}} @{{.*}}(ptr [[OBJECT]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 4
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret void

//===----------------------------------------------------------------------===//
// ConcreteVectorMovesAsLike initializeWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout25ConcreteVectorMovesAsLikeVwtk"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[DEST_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout25ConcreteVectorMovesAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout25ConcreteVectorMovesAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[SRC_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[SRC_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    [[DEST_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[DEST_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_0]], ptr [[SRC_0]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 4
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest

//===----------------------------------------------------------------------===//
// ConcreteVectorMovesAsLike assignWithTake
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s10raw_layout25ConcreteVectorMovesAsLikeVwta"(ptr {{.*}} %dest, ptr {{.*}} %src, ptr %ConcreteVectorMovesAsLike)
// CHECK:         [[I_ALLOCA:%.*]] = alloca {{i64|i32}}
// CHECK:         [[DEST_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout25ConcreteVectorMovesAsLikeV, ptr %dest, i32 0, i32 0
// CHECK:         [[SRC_VECTOR:%.*]] = getelementptr inbounds{{.*}} %T10raw_layout25ConcreteVectorMovesAsLikeV, ptr %src, i32 0, i32 0
// CHECK:         store {{i64|i32}} 0, ptr [[I_ALLOCA]]
// CHECK:         br label %[[COND_BR:.*]]

// CHECK:       [[COND_BR]]:
// CHECK:         [[I:%.*]] = load {{i64|i32}}, ptr [[I_ALLOCA]]
// CHECK:         br i1 true, label %[[LOOP_BR:.*]], label %[[EXIT_BR:.*]]

// CHECK:       [[LOOP_BR]]:
// CHECK:         [[NEW_I:%.*]] = add {{i64|i32}} [[I]], 1
// CHECK:         store {{i64|i32}} [[NEW_I]], ptr [[I_ALLOCA]]
// CHECK:         [[SRC_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[SRC_VECTOR]], {{i64|i32}} [[I]]
// CHECK-NEXT:    [[DEST_0:%.*]] = getelementptr inbounds %TSo24NonBitwiseTakableCXXTypeV, ptr [[DEST_VECTOR]], {{i64|i32}} [[I]]
// CHECK:         {{invoke void|invoke ptr|call ptr}} @{{.*}}(ptr [[DEST_0]], ptr [[SRC_0]])

// This may or may not be in the loop_br
// CHECK:         [[EQ_CMP:%.*]] = icmp eq {{i64|i32}} [[NEW_I]], 4
// CHECK:         br i1 [[EQ_CMP]], label %[[EXIT_BR]], label %[[COND_BR]]

// CHECK:       [[EXIT_BR]]:
// CHECK:         ret ptr %dest
