// RUN: %target-swift-frontend -Onone -target %target-swift-5.9-abi-triple -emit-ir -primary-file %s -enable-pack-metadata-stack-promotion=false -enable-pack-metadata-stack-promotion=true -Xllvm -sil-print-after=pack-metadata-marker-inserter 2>&1 | %FileCheck %s --check-prefixes CHECK-SIL
// RUN: %target-swift-frontend -Onone -target %target-swift-5.9-abi-triple -emit-ir -primary-file %s -enable-pack-metadata-stack-promotion=false -enable-pack-metadata-stack-promotion=true | %IRGenFileCheck %s --check-prefixes CHECK-LLVM

public struct G<each T> {
  var pack: (repeat each T)
}

public struct S<T> {
  let g: G<T>
}

// CHECK-SIL-LABEL: sil @consumeS : {{.*}} {
// CHECK-SIL:       bb0([[INSTANCE:%[^,]+]] :
// CHECK-SIL:         alloc_pack_metadata
// CHECK-SIL:         [[METADATA:%[^,]+]] = alloc_pack_metadata
// CHECK-SIL:         destroy_addr [[INSTANCE]]
// CHECK-SIL:         dealloc_pack_metadata [[METADATA]]
// CHECK-SIL-LABEL: } // end sil function 'consumeS'
// CHECK-LLVM-LABEL: define{{.*}} swiftcc void @consumeS(
// CHECK-LLVM-SAME:      ptr noalias [[INSTANCE:%[^,]+]], 
// CHECK-LLVM-SAME:      ptr [[T_METADATA:%[^)]+]]
// CHECK-LLVM-SAME:  ) {{.*}} {
// CHECK-LLVM:         [[G_METADATA_PACK:%[^,]+]] = alloca [1 x ptr]
// CHECK-LLVM:         call void @llvm.lifetime.start.p0(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:         [[G_METADATA_PACK_T_SLOT:%[^,]+]] = getelementptr inbounds{{.*}} [1 x ptr], ptr [[G_METADATA_PACK]]
// CHECK-LLVM:         store ptr [[T_METADATA]], ptr [[G_METADATA_PACK_T_SLOT]]
// CHECK-LLVM:         [[G_METADATA_RESPONSE:%[^,]+]] = call swiftcc %swift.metadata_response @"$s35pack_metadata_marker_inserter_onone1GVMa"(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:         [[G_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[G_METADATA_RESPONSE]]
// CHECK-LLVM:         [[S_METADATA_RESPONSE:%[^,]+]] = call swiftcc %swift.metadata_response @"$s35pack_metadata_marker_inserter_onone1SVMa"(
// CHECK-LLVM-SAME:        ptr [[T_METADATA]])
// CHECK-LLVM:         [[S_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[S_METADATA_RESPONSE]]
//                     The metadata for G<T> is passed to the outlined destroy for S.
// CHECK-LLVM:         @"$s35pack_metadata_marker_inserter_onone1SVyxGlWOh"(ptr [[INSTANCE]], ptr [[T_METADATA]], ptr [[G_METADATA]], ptr [[S_METADATA]])
// CHECK-LLVM:         call void @llvm.lifetime.end.p0(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:       }
@_silgen_name("consumeS")
public func consumeS<T>(_: consuming S<T>) {}

public func callConsumeS<T>(_ s: consuming S<T>) {
  consumeS(s)
}

@_silgen_name("consume2S")
public func consume2S<T>(_: consuming S<T>, _: consuming S<T>) {}

// CHECK-SIL-LABEL: sil @callConsume2S : {{.*}} {
// CHECK-SIL:       bb0([[INSTANCE:%[^,]+]] :
// CHECK-SIL:         alloc_pack_metadata
// CHECK-SIL:         [[COPY2:%[^,]+]] = alloc_stack
// CHECK-SIL:         alloc_pack_metadata
// CHECK-SIL:         [[COPY1:%[^,]+]] = alloc_stack
// CHECK-SIL:         alloc_pack_metadata
// CHECK-SIL:         [[COPY_ADDR_1_METADATA:%[^,]+]] = alloc_pack_metadata
// CHECK-SIL:         copy_addr [[INSTANCE]] to [init] [[COPY1]]
// CHECK-SIL:         [[COPY_ADDR_2_METADATA:%[^,]+]] = alloc_pack_metadata
// CHECK-SIL:         copy_addr [[INSTANCE]] to [init] [[COPY2]]
// CHECK-SIL:         [[CONSUME_2_S:%[^,]+]] = function_ref @consume2S
// CHECK-SIL:         apply [[CONSUME_2_S]]<T>([[COPY1]], [[COPY2]])
// CHECK-SIL:         dealloc_pack_metadata [[COPY_ADDR_2_METADATA]]
// CHECK-SIL:         dealloc_pack_metadata [[COPY_ADDR_1_METADATA]]
// CHECK-SIL-LABEL: } // end sil function 'callConsume2S'
// CHECK-LLVM-LABEL: define{{.*}} swiftcc void @callConsume2S(
// CHECK-LLVM-SAME:      ptr noalias [[INSTANCE:%[^,]+]], 
// CHECK-LLVM-SAME:      ptr [[T_METADATA:%[^)]+]]
// CHECK-LLVM-SAME:  ) {{.*}} {
// CHECK-LLVM:         [[G_METADATA_PACK:%[^,]+]] = alloca [1 x ptr]
// CHECK-LLVM:         [[S_METADATA_RESPONSE:%[^,]+]] = call swiftcc %swift.metadata_response @"$s35pack_metadata_marker_inserter_onone1SVMa"(
// CHECK-LLVM-SAME:        ptr [[T_METADATA]])
// CHECK-LLVM:         [[S_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[S_METADATA_RESPONSE]]
// CHECK-LLVM:         [[S_VWT_ADDR:%[^,]+]] = getelementptr inbounds ptr, ptr [[S_METADATA]], [[INT]] -1
// CHECK-LLVM:         [[S_VWT:%[^,]+]] = load ptr, ptr [[S_VWT_ADDR]]
// CHECK-LLVM:         [[S_SIZE_ADDR:%[^,]+]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr 
//  HECK-LLVM-SAME:        [[S_VWT]]
// CHECK-LLVM:         [[S_SIZE:%[^,]+]] = load [[INT]], ptr [[S_SIZE_ADDR]]
// CHECK-LLVM:         [[COPY_1_ADDR:%[^,]+]] = alloca i8, [[INT]] [[S_SIZE]]
// CHECK-LLVM:         call void @llvm.lifetime.start.p0(
// CHECK-LLVM-SAME:        ptr [[COPY_1_ADDR]])
// CHECK-LLVM:         [[COPY_2_ADDR:%[^,]+]] = alloca i8, [[INT]] [[S_SIZE]]
// CHECK-LLVM:         call void @llvm.lifetime.start.p0(
// CHECK-LLVM-SAME:        ptr [[COPY_2_ADDR]])
// CHECK-LLVM:         call void @llvm.lifetime.start.p0(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:         [[G_METADATA_PACK_T_SLOT:%[^,]+]] = getelementptr inbounds{{.*}} [1 x ptr], ptr [[G_METADATA_PACK]]
// CHECK-LLVM:         store ptr [[T_METADATA]], ptr [[G_METADATA_PACK_T_SLOT]]
// CHECK-LLVM:         [[G_METADATA_RESPONSE:%[^,]+]] = call swiftcc %swift.metadata_response @"$s35pack_metadata_marker_inserter_onone1GVMa"(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:         [[G_METADATA:%[^,]+]] = extractvalue %swift.metadata_response [[G_METADATA_RESPONSE]], 0
// CHECK-LLVM:         call ptr @"$s35pack_metadata_marker_inserter_onone1SVyxGlWOc"(ptr [[INSTANCE]], ptr [[COPY_2_ADDR]], ptr [[T_METADATA]], ptr [[G_METADATA]], ptr [[S_METADATA]])
// CHECK-LLVM:         call ptr @"$s35pack_metadata_marker_inserter_onone1SVyxGlWOc"(ptr [[INSTANCE]], ptr [[COPY_1_ADDR]], ptr [[T_METADATA]], ptr [[G_METADATA]], ptr [[S_METADATA]])
// CHECK-LLVM:         call swiftcc void @consume2S(ptr noalias [[COPY_2_ADDR]], ptr noalias [[COPY_1_ADDR]], ptr [[T_METADATA]])
// CHECK-LLVM:         call void @llvm.lifetime.end.p0(
// CHECK-LLVM-SAME:        ptr [[G_METADATA_PACK]])
// CHECK-LLVM:         call void @llvm.lifetime.end.p0(
// CHECK-LLVM-SAME:        ptr [[COPY_2_ADDR]])
// CHECK-LLVM:         call void @llvm.lifetime.end.p0(
// CHECK-LLVM-SAME:        ptr [[COPY_1_ADDR]])
// CHECK-LLVM-LABEL: }
@_silgen_name("callConsume2S")
public func callConsume2S<T>(_ s: S<T>) {
  consume2S(s, s)
}
