// RUN: %target-swift-emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:   -target %target-swift-5.8-abi-triple -Xcc -fignore-exceptions \
// RUN:   -enable-experimental-feature ForeignReferenceTypeSubclassing \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-ptrauth,CHECK-%target-abi,CHECK-%target-ptrauth-%target-abi

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

import InheritFRTSubclassing

// CHECK: %T{{.*}}3SubC = type <{ [8 x i8], %Ts5Int32V, %Ts5Int32V, %Ts5Int64V }>

// CHECK: %T{{.*}}5MultiC = type <{ [8 x i8], %Ts5Int32V, %Ts5Int32V, %Ts4Int8V, [7 x i8], %Ts5Int64V }>

// The Microsoft ABI allocates a bit-field in a whole storage unit of its type.
// CHECK-SYSV: %T{{.*}}15BitfieldLastSubC = type <{ [8 x i8], %Ts5Int32V, [2 x i8], %Ts4Int8V }>
// CHECK-WIN:  %T{{.*}}15BitfieldLastSubC = type <{ [8 x i8], %Ts5Int32V, [4 x i8], %Ts4Int8V }>
// CHECK: %T{{.*}}22UnimportedFieldLastSubC = type <{ [8 x i8], %Ts5Int32V, [4 x i8], %Ts4Int8V }>
// CHECK: %T{{.*}}20VTablePointerOnlySubC = type <{ [8 x i8], %Ts4Int8V }>

// Stored properties start in the tail padding of a C++ base, which the
// Microsoft ABI does not reuse.
// CHECK-SYSV: %T{{.*}}13TailPaddedSubC = type <{ [8 x i8], %Ts5Int32V, %Ts5Int32V, %Ts5Int32V }>
// CHECK-WIN:  %T{{.*}}13TailPaddedSubC = type <{ [8 x i8], %Ts5Int32V, [4 x i8], %Ts5Int32V, %Ts5Int32V }>

// CHECK-SYSV: @_ZTV19__SwiftSubclassShimI20ReferenceConstructed{{.*}} = linkonce_odr unnamed_addr constant { [4 x ptr] }
// CHECK-SYSV: @_ZTV19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}} = linkonce_odr unnamed_addr constant { [4 x ptr], [5 x ptr] }

// CHECK-SYSV: @"$s{{.*}}3SubCMf" = internal constant <{ ptr, ptr, i64, ptr, ptr }> <{ ptr null, ptr @"$s{{.*}}3SubCWV", i64 516, ptr @"$s{{.*}}3SubCMn", ptr null }>
// CHECK-WIN:  @"$s{{.*}}3SubCMf" = internal global <{ ptr, ptr, i64, ptr, ptr }> <{ ptr null, ptr @"$s{{.*}}3SubCWV", i64 516, ptr @"$s{{.*}}3SubCMn", ptr null }>

// CHECK-WIN: @"??_7?$__SwiftSubclassShim@UReferenceConstructed@@{{.*}}@@6B@" = alias ptr
// CHECK-WIN: @"??_7?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@6BSubclassableShared@@@" = alias ptr
// CHECK-WIN: @"??_7?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@6BSecondaryPolymorphic@@@" = alias ptr

// CHECK: @"$s{{.*}}3SubCN" = {{(dllexport )?}}alias %swift.type, getelementptr inbounds (<{ ptr, ptr, i64, ptr, ptr }>, ptr @"$s{{.*}}3SubCMf", i32 0, i32 2)

public final class Sub: SubclassableShared {
  public let x: Int64 = 42
}

public final class Multi: SubclassableShared {
  public let a: Int8 = 1
  public let b: Int64 = 2
}

public final class BitfieldLastSub: BitfieldLastShared {
  public var x: Int8 = 0
}

public final class UnimportedFieldLastSub: UnimportedFieldLastShared {
  public var x: Int8 = 0
}

public final class VTablePointerOnlySub: VTablePointerOnlyShared {
  public var x: Int8 = 0
}

public func getX(_ s: Sub) -> Int64 { return s.x }
// CHECK-LABEL: define {{.*}}4getX
// CHECK: getelementptr inbounds{{.*}} %T{{.*}}3SubC, ptr %0, i32 0, i32 3


public final class SwiftSubShared: SubclassableShared {}

public final class SubOfDerivedShared: DerivedOwnRefcountShared {}

public func retainRelease(_ x: SwiftSubShared) -> (SwiftSubShared, SwiftSubShared) {
  return (x, x)
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}13retainRelease{{.*}}"(ptr %0)
// CHECK-NOT: swift_retain
// CHECK: call void @{{.*}}retainSubclassableShared{{.*}}(ptr %0)
// CHECK: call void @{{.*}}retainSubclassableShared{{.*}}(ptr %0)
// CHECK: ret

public func consume(_ x: consuming SwiftSubShared) {}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}7consume{{.*}}"(ptr %0)
// CHECK-NOT: swift_release
// CHECK: call void @{{.*}}releaseSubclassableShared{{.*}}(ptr %{{.*}})
// CHECK: ret void

public func consumeDerived(_ x: consuming SubOfDerivedShared) {}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}14consumeDerived{{.*}}"(ptr %0)
// CHECK-NOT: swift_release
// CHECK: call void @{{.*}}releaseDerivedOwnRefcountShared{{.*}}(ptr %{{.*}})
// CHECK: ret void

public final class ConstRefArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ i: Int32) { super.init(i) }
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}14ConstRefArgSubCyACs5Int32Vcfc"(i32 %0,
// CHECK:         [[TEMP:%.*]] = alloca %Ts5Int32V
// CHECK-NOT:     inttoptr
// CHECK-SYSV-NOT: _ZN20ReferenceConstructedC2ERKi
// CHECK-WIN-NOT:  ??0ReferenceConstructed@@QEAA@AEBH@Z
// CHECK-SYSV:    [[SELF:%[0-9]+]] = call ptr @__swift_cxx_base_ctor_ZN20ReferenceConstructedC2ERKi(ptr {{%[0-9]+}}, ptr [[TEMP]])
// CHECK-WIN:     [[SELF:%[0-9]+]] = call ptr @"__swift_cxx_base_ctor??0ReferenceConstructed@@QEAA@AEBH@Z"(ptr {{%[0-9]+}}, ptr [[TEMP]])
// CHECK-SYSV-NEXT: call void @[[CR_INIT:__clang_vtable_ptr_init\._ZTS19__SwiftSubclassShimI20ReferenceConstructed[^(]*]](ptr [[SELF]])
// CHECK-WIN-NEXT:  call void @[[CR_INIT:"__clang_vtable_ptr_init\.\?AU\?\$__SwiftSubclassShim@UReferenceConstructed@@[^"]*"]](ptr [[SELF]])
// CHECK:         ret

// CHECK-SYSV:  define private ptr @__swift_cxx_base_ctor_ZN20ReferenceConstructedC2ERKi(ptr %0, ptr %1)
// CHECK-WIN:   define private ptr @"__swift_cxx_base_ctor??0ReferenceConstructed@@QEAA@AEBH@Z"(ptr %0, ptr %1)
// CHECK-NEXT:  entry:
// CHECK-SYSV-NEXT: call ptr @_ZN20ReferenceConstructedC2ERKi(ptr %0, ptr %1)
// CHECK-WIN-NEXT:  call ptr @"??0ReferenceConstructed@@QEAA@AEBH@Z"(ptr %0, ptr %1)
// CHECK-NEXT:    ret ptr %0

// CHECK-SYSV:  define linkonce_odr void @_ZN19__SwiftSubclassShimI20ReferenceConstructed{{.*}}D0Ev(
// CHECK:       define linkonce_odr hidden void @[[CR_INIT]](ptr {{.*}})
// CHECK:         [[THIS:%.*]] = load ptr, ptr
// CHECK:         [[VPTR:%.*]] = getelementptr inbounds i8, ptr [[THIS]], i64 0
// CHECK-noptrauth-SYSV-NEXT: store ptr getelementptr inbounds inrange(-16, 16) (i8, ptr @_ZTV19__SwiftSubclassShimI20ReferenceConstructed{{.*}}, i64 16), ptr [[VPTR]]
// CHECK-WIN-NEXT:       store ptr @"??_7?$__SwiftSubclassShim@UReferenceConstructed@@{{.*}}@@6B@", ptr [[VPTR]]
// CHECK-ptrauth-NEXT:   [[VPTR_ADDR:%.*]] = ptrtoint ptr [[VPTR]] to i64
// CHECK-ptrauth-NEXT:   [[VPTR_DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[VPTR_ADDR]], i64 24441)
// CHECK-ptrauth-NEXT:   [[VPTR_SIGNED:%.*]] = call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr getelementptr inbounds inrange(-16, 16) (i8, ptr @_ZTV19__SwiftSubclassShimI20ReferenceConstructed{{.*}}, i64 16) to i64), i32 2, i64 [[VPTR_DISC]])
// CHECK-ptrauth-NEXT:   [[VPTR_VAL:%.*]] = inttoptr i64 [[VPTR_SIGNED]] to ptr
// CHECK-ptrauth-NEXT:   store ptr [[VPTR_VAL]], ptr [[VPTR]]
// CHECK-NEXT:    ret void

public final class ByValueArgSub: ReferenceConstructed {
  public let t: Int64 = 1
  public init(_ arg: NonTrivialArg) { super.init(arg, CLong(0)) }
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}13ByValueArgSubCyACSo010NonTrivialD0Vcfc"(ptr
// CHECK:         [[TEMP:%.*]] = alloca %TSo13NonTrivialArgV
// CHECK-SYSV:    call {{.*}} @_ZN13NonTrivialArgC1ERKS_(ptr [[TEMP]],
// CHECK-SYSV-NEXT: call ptr @__swift_cxx_base_ctor_ZN20ReferenceConstructedC2E13NonTrivialArgl(ptr {{%.*}}, ptr [[TEMP]], i64 0)
// CHECK-SYSV-NEXT: call {{.*}} @_ZN13NonTrivialArgD1Ev(ptr [[TEMP]])
// In the Microsoft ABI, the callee destroys an argument passed by value.
// CHECK-WIN:     call {{.*}} @"??0NonTrivialArg@@QEAA@AEBU0@@Z"(ptr [[TEMP]],
// CHECK-WIN-NEXT:  call ptr @"__swift_cxx_base_ctor??0ReferenceConstructed@@QEAA@UNonTrivialArg@@J@Z"(ptr {{%.*}}, ptr [[TEMP]], i32 0)
// CHECK-WIN-NOT:   @"??1NonTrivialArg@@QEAA@XZ"(ptr [[TEMP]])

// CHECK-SYSV:  define private ptr @__swift_cxx_base_ctor_ZN20ReferenceConstructedC2E13NonTrivialArgl(ptr %0, ptr %1, i64 %2)
// CHECK-WIN:   define private ptr @"__swift_cxx_base_ctor??0ReferenceConstructed@@QEAA@UNonTrivialArg@@J@Z"(ptr %0, ptr %1, i32 %2)
// CHECK-NEXT:  entry:
// CHECK-SYSV-NEXT: call ptr @_ZN20ReferenceConstructedC2E13NonTrivialArgl(ptr %0, ptr %1, i64 %2)
// CHECK-WIN-NEXT:  call ptr @"??0ReferenceConstructed@@QEAA@UNonTrivialArg@@J@Z"(ptr %0, ptr %1, i32 %2)
// CHECK-NEXT:    ret ptr %0

// With pointer authentication, the v-table pointers are signed with the address
// of the v-table pointer blended with a discriminator of the class that
// introduced it.
public final class MultiplyInheritedSub: MultiplyInheritedShared {
  public let z: Int64 = 3
  public init() { super.init() }
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}20MultiplyInheritedSubCACycfc"(ptr
// CHECK-SYSV-NOT: _ZN23MultiplyInheritedSharedC2Ev
// CHECK-WIN-NOT:  ??0MultiplyInheritedShared@@QEAA@XZ
// CHECK-SYSV:    [[SELF:%[0-9]+]] = call ptr @__swift_cxx_base_ctor_ZN23MultiplyInheritedSharedC2Ev(ptr {{%[0-9]+}})
// CHECK-WIN:     [[SELF:%[0-9]+]] = call ptr @"__swift_cxx_base_ctor??0MultiplyInheritedShared@@QEAA@XZ"(ptr {{%[0-9]+}})
// CHECK-NEXT:    call void @{{.*}}retainMultiplyInheritedShared{{.*}}(ptr [[SELF]])
// CHECK-SYSV-NEXT: call void @[[MI_INIT:__clang_vtable_ptr_init\._ZTS19__SwiftSubclassShimI23MultiplyInheritedShared[^(]*]](ptr [[SELF]])
// CHECK-WIN-NEXT:  call void @[[MI_INIT:"__clang_vtable_ptr_init\.\?AU\?\$__SwiftSubclassShim@UMultiplyInheritedShared@@[^"]*"]](ptr [[SELF]])
// CHECK:         ret

// CHECK-SYSV:  define private ptr @__swift_cxx_base_ctor_ZN23MultiplyInheritedSharedC2Ev(ptr %0)
// CHECK-WIN:   define private ptr @"__swift_cxx_base_ctor??0MultiplyInheritedShared@@QEAA@XZ"(ptr %0)
// CHECK-NEXT:  entry:
// CHECK-SYSV-NEXT: call ptr @_ZN23MultiplyInheritedSharedC2Ev(ptr %0)
// CHECK-WIN-NEXT:  call ptr @"??0MultiplyInheritedShared@@QEAA@XZ"(ptr %0)
// CHECK-NEXT:    ret ptr %0

// CHECK-SYSV: define linkonce_odr void @_ZN19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}D0Ev(
// CHECK-SYSV: define linkonce_odr void @_ZThn16_N19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}D0Ev(
// CHECK-WIN:  define linkonce_odr {{.*}} @"??_E?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@WBA@EAAPEAXI@Z"(

// The v-table pointer at offset 0 is discriminated by `SubclassableShared`,
// the root of its primary base chain, and the one at offset 16 by
// `SecondaryPolymorphic`.
// CHECK:       define linkonce_odr hidden void @[[MI_INIT]](ptr
// CHECK:         [[THIS:%.*]] = load ptr, ptr
// CHECK:         [[PRIMARY:%.*]] = getelementptr inbounds i8, ptr [[THIS]], i64 0
// CHECK-noptrauth-SYSV-NEXT: store ptr getelementptr inbounds inrange(-16, 16) (i8, ptr @_ZTV19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}, i64 16), ptr [[PRIMARY]]
// CHECK-WIN-NEXT:       store ptr @"??_7?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@6BSubclassableShared@@@", ptr [[PRIMARY]]
// CHECK-ptrauth-NEXT:   [[PRIMARY_ADDR:%.*]] = ptrtoint ptr [[PRIMARY]] to i64
// CHECK-ptrauth-NEXT:   [[PRIMARY_DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[PRIMARY_ADDR]], i64 16555)
// CHECK-ptrauth-NEXT:   [[PRIMARY_SIGNED:%.*]] = call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr getelementptr inbounds inrange(-16, 16) (i8, ptr @_ZTV19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}, i64 16) to i64), i32 2, i64 [[PRIMARY_DISC]])
// CHECK-ptrauth-NEXT:   [[PRIMARY_VPTR:%.*]] = inttoptr i64 [[PRIMARY_SIGNED]] to ptr
// CHECK-ptrauth-NEXT:   store ptr [[PRIMARY_VPTR]], ptr [[PRIMARY]]
// CHECK-NEXT:    [[SECONDARY:%.*]] = getelementptr inbounds i8, ptr [[THIS]], i64 16
// CHECK-noptrauth-SYSV-NEXT: store ptr getelementptr inbounds inrange(-16, 24) (i8, ptr @_ZTV19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}, i64 48), ptr [[SECONDARY]]
// CHECK-WIN-NEXT:       store ptr @"??_7?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@6BSecondaryPolymorphic@@@", ptr [[SECONDARY]]
// CHECK-ptrauth-NEXT:   [[SECONDARY_ADDR:%.*]] = ptrtoint ptr [[SECONDARY]] to i64
// CHECK-ptrauth-NEXT:   [[SECONDARY_DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[SECONDARY_ADDR]], i64 63745)
// CHECK-ptrauth-NEXT:   [[SECONDARY_SIGNED:%.*]] = call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr getelementptr inbounds inrange(-16, 24) (i8, ptr @_ZTV19__SwiftSubclassShimI23MultiplyInheritedShared{{.*}}, i64 48) to i64), i32 2, i64 [[SECONDARY_DISC]])
// CHECK-ptrauth-NEXT:   [[SECONDARY_VPTR:%.*]] = inttoptr i64 [[SECONDARY_SIGNED]] to ptr
// CHECK-ptrauth-NEXT:   store ptr [[SECONDARY_VPTR]], ptr [[SECONDARY]]
// CHECK-NEXT:    ret void

public final class TailPaddedSub: TailPaddedShared {
  public var y: Int32 = 5
  public var z: Int32 = 6
  public init() { super.init() }
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}13TailPaddedSubCACycfC"()
// CHECK-SYSV:    call {{.*}} @[[ALLOC:_ZN19__SwiftSubclassShimI16TailPaddedShared.*16__swift_allocateEv]]()
// CHECK-WIN:     call {{.*}} @[[ALLOC:"\?__swift_allocate@\?\$__SwiftSubclassShim@UTailPaddedShared@@[^"]*"]]()
// CHECK:       define linkonce_odr {{.*}} @[[ALLOC]]()
// CHECK-SYSV:    call {{.*}} @[[NEW:_ZN29__swift_subclassShimAllocatorI19__SwiftSubclassShimI16TailPaddedShared[^(]*]](
// CHECK-WIN:     call {{.*}} @[[NEW:"\?allocate@\?\$__swift_subclassShimAllocator@U\?\$__SwiftSubclassShim@UTailPaddedShared@@[^"]*"]](

public final class ClassAllocatedSub: ClassAllocatedShared {
  public var x: Int64 = 1
  public init() { super.init() }
}
// CHECK-LABEL: define {{.*}} @"$s{{.*}}17ClassAllocatedSubCACycfC"()
// CHECK-SYSV:    call {{.*}} @[[CA_ALLOC:_ZN19__SwiftSubclassShimI20ClassAllocatedShared.*16__swift_allocateEv]]()
// CHECK-WIN:     call {{.*}} @[[CA_ALLOC:"\?__swift_allocate@\?\$__SwiftSubclassShim@UClassAllocatedShared@@[^"]*"]]()
// CHECK:       define linkonce_odr {{.*}} @[[CA_ALLOC]]()
// CHECK-SYSV:    call {{.*}} @[[CA_NEW:_ZN29__swift_subclassShimAllocatorI19__SwiftSubclassShimI20ClassAllocatedShared[^(]*]](
// CHECK-WIN:     call {{.*}} @[[CA_NEW:"\?allocate@\?\$__swift_subclassShimAllocator@U\?\$__SwiftSubclassShim@UClassAllocatedShared@@[^"]*"]](
// CHECK-SYSV:  define linkonce_odr void @_ZN19__SwiftSubclassShimI20ClassAllocatedShared{{.*}}D0Ev(
// CHECK-SYSV:    call void @_ZN20ClassAllocatedShareddlEPv(

// CHECK-LABEL: define {{.*}} @"$s{{.*}}3SubCMa"(i64 %0)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    ret %swift.metadata_response { ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, ptr }>, ptr @"$s{{.*}}3SubCMf", i32 0, i32 2), i64 0 }

// CHECK-WIN:   define linkonce_odr {{.*}} @"??_G?$__SwiftSubclassShim@UReferenceConstructed@@{{.*}}@@UEAAPEAXI@Z"(
// CHECK-WIN:   define linkonce_odr {{.*}} @"??_G?$__SwiftSubclassShim@UMultiplyInheritedShared@@{{.*}}@@UEAAPEAXI@Z"(

// CHECK:       define linkonce_odr {{.*}} @[[NEW]](
// CHECK-SYSV:    call {{.*}} @_Znwm(i64 noundef 24)
// CHECK-WIN:     call {{.*}} @"??2@YAPEAX_K@Z"(i64 noundef 24)
// CHECK:       define linkonce_odr {{.*}} @[[CA_NEW]](
// CHECK-SYSV:    call {{.*}} @_ZN20ClassAllocatedSharednwEm(i64 noundef 32)
// CHECK-WIN:     call {{.*}} @"??2ClassAllocatedShared@@SAPEAX_K@Z"(i64 noundef 24)
// CHECK-WIN:   define linkonce_odr {{.*}} @"??_G?$__SwiftSubclassShim@UClassAllocatedShared@@{{.*}}@@UEAAPEAXI@Z"(
// CHECK-WIN:     call void @"??3ClassAllocatedShared@@SAXPEAX@Z"(
