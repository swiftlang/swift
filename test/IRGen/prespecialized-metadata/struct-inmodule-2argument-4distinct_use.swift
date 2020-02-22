// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main5ValueVys5UInt8VSSGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast (%swift.opaque* ([{{[0-9]+}} x i8]*, [{{[0-9]+}} x i8]*, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwCP" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwxx" to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwcp" to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwca" to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwta" to i8*),
// CHECK-SAME:    i8* bitcast (i32 (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwet" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, i32, i32, %swift.type*)* @"$s4main5ValueVys5UInt8VSSGwst" to i8*),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore COMDAT on PE/COFF targets
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVys5UInt8VSSGMf" = linkonce_odr hidden constant <{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }> <{ i8** getelementptr inbounds (%swift.vwtable, %swift.vwtable* @"$s4main5ValueVys5UInt8VSSGWV", i32 0, i32 0), [[INT]] 512, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*), %swift.type* @"$ss5UInt8VN", %swift.type* @"$sSSN", i32 0, i32 [[ALIGNMENT]], i64 3 }>, align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVySSSdGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast (%swift.opaque* ([{{[0-9]+}} x i8]*, [{{[0-9]+}} x i8]*, %swift.type*)* @"$s4main5ValueVySSSdGwCP" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, %swift.type*)* @"$s4main5ValueVySSSdGwxx" to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVySSSdGwcp" to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVySSSdGwca" to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (%swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)* @"$s4main5ValueVySSSdGwta" to i8*),
// CHECK-SAME:    i8* bitcast (i32 (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueVySSSdGwet" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, i32, i32, %swift.type*)* @"$s4main5ValueVySSSdGwst" to i8*),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore COMDAT on PE/COFF targets
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVySSSdGMf" = linkonce_odr hidden constant <{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }> <{ i8** getelementptr inbounds (%swift.vwtable, %swift.vwtable* @"$s4main5ValueVySSSdGWV", i32 0, i32 0), [[INT]] 512, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*), %swift.type* @"$sSSN", %swift.type* @"$sSdN", i32 0, i32 16, i64 3 }>, align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVySdSiGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9_]+" to i8\*|i8\* \(i8\*, i8\*, %swift.type\*\)\* @__swift_memcpy[0-9]+_[0-9]+ to i8\*)}}),
// CHECK-SAME:    i8* bitcast (void (i8*, %swift.type*)* @__swift_noop_void_return to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i32 (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueVySdSiGwet" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, i32, i32, %swift.type*)* @"$s4main5ValueVySdSiGwst" to i8*),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore COMDAT on PE/COFF targets
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVySdSiGMf" = linkonce_odr hidden constant <{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }> <{ i8** getelementptr inbounds (%swift.vwtable, %swift.vwtable* @"$s4main5ValueVySdSiGWV", i32 0, i32 0), [[INT]] 512, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*), %swift.type* @"$sSdN", %swift.type* @"$sSiN", i32 0, i32 8, i64 3 }>, align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVyS2iGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9_]+" to i8\*|i8\* \(i8\*, i8\*, %swift.type\*\)\* @__swift_memcpy[0-9]+_[0-9]+ to i8\*)}}),
// CHECK-SAME:    i8* bitcast (void (i8*, %swift.type*)* @__swift_noop_void_return to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*),
// CHECK-SAME:    i8* bitcast (i32 (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueVyS2iGwet" to i8*),
// CHECK-SAME:    i8* bitcast (void (%swift.opaque*, i32, i32, %swift.type*)* @"$s4main5ValueVyS2iGwst" to i8*),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore COMDAT on PE/COFF targets
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVyS2iGMf" = linkonce_odr hidden constant <{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }> <{ i8** getelementptr inbounds (%swift.vwtable, %swift.vwtable* @"$s4main5ValueVyS2iGWV", i32 0, i32 0), [[INT]] 512, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*), %swift.type* @"$sSiN", %swift.type* @"$sSiN", i32 0, i32 [[ALIGNMENT]], i64 3 }>, align [[ALIGNMENT]]
struct Value<First, Second> {
  let first: First
  let second: Second
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVyS2iGMf" to %swift.full_type*), i32 0, i32 1))
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVySdSiGMf" to %swift.full_type*), i32 0, i32 1))
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVySSSdGMf" to %swift.full_type*), i32 0, i32 1))
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVys5UInt8VSSGMf" to %swift.full_type*), i32 0, i32 1))
// CHECK: }
func doit() {
  consume( Value(first: 13, second: 13) )
  consume( Value(first: 13.0, second: 13) )
  consume( Value(first: "13.0", second: 13.0) )
  consume( Value(first: 13 as UInt8, second: "13.0") )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind readnone
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueVMa"([[INT]] %0, %swift.type* %1, %swift.type* %2) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_TYPE_1:%[0-9]+]] = bitcast %swift.type* %1 to i8*
// CHECK:   [[ERASED_TYPE_2:%[0-9]+]] = bitcast %swift.type* %2 to i8*
// CHECK:   br label %[[TYPE_COMPARISON_1:[0-9]+]]
// CHECK: [[TYPE_COMPARISON_1]]:
// CHECK:   [[EQUAL_TYPE_1_1:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_1]]
// CHECK:   [[EQUAL_TYPES_1_1:%[0-9]+]] = and i1 true, [[EQUAL_TYPE_1_1]]
// CHECK:   [[EQUAL_TYPE_1_2:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_2]]
// CHECK:   [[EQUAL_TYPES_1_2:%[0-9]+]] = and i1 [[EQUAL_TYPES_1_1]], [[EQUAL_TYPE_1_2]]
// CHECK:   br i1 [[EQUAL_TYPES_1_2]], label %[[EXIT_PRESPECIALIZED_1:[0-9]+]], label %[[TYPE_COMPARISON_2:[0-9]+]]
// CHECK: [[TYPE_COMPARISON_2]]:
// CHECK:   [[EQUAL_TYPE_2_1:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSdN" to i8*), [[ERASED_TYPE_1]]
// CHECK:   [[EQUAL_TYPES_2_1:%[0-9]+]] = and i1 true, [[EQUAL_TYPE_2_1]]
// CHECK:   [[EQUAL_TYPE_2_2:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_2]]
// CHECK:   [[EQUAL_TYPES_2_2:%[0-9]+]] = and i1 [[EQUAL_TYPES_2_1]], [[EQUAL_TYPE_2_2]]
// CHECK:   br i1 [[EQUAL_TYPES_2_2]], label %[[EXIT_PRESPECIALIZED_2:[0-9]+]], label %[[TYPE_COMPARISON_3:[0-9]+]]
// CHECK: [[TYPE_COMPARISON_3]]:
// CHECK:   [[EQUAL_TYPE_3_1:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSSN" to i8*), [[ERASED_TYPE_1]]
// CHECK:   [[EQUAL_TYPES_3_1:%[0-9]+]] = and i1 true, [[EQUAL_TYPE_3_1]]
// CHECK:   [[EQUAL_TYPE_3_2:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSdN" to i8*), [[ERASED_TYPE_2]]
// CHECK:   [[EQUAL_TYPES_3_2:%[0-9]+]] = and i1 [[EQUAL_TYPES_3_1]], [[EQUAL_TYPE_3_2]]
// CHECK:   br i1 [[EQUAL_TYPES_3_2]], label %[[EXIT_PRESPECIALIZED_3:[0-9]+]], label %[[TYPE_COMPARISON_4:[0-9]+]]
// CHECK: [[TYPE_COMPARISON_4]]:
// CHECK:   [[EQUAL_TYPE_4_1:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$ss5UInt8VN" to i8*), [[ERASED_TYPE_1]]
// CHECK:   [[EQUAL_TYPES_4_1:%[0-9]+]] = and i1 true, [[EQUAL_TYPE_4_1]]
// CHECK:   [[EQUAL_TYPE_4_2:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSSN" to i8*), [[ERASED_TYPE_2]]
// CHECK:   [[EQUAL_TYPES_4_2:%[0-9]+]] = and i1 [[EQUAL_TYPES_4_1]], [[EQUAL_TYPE_4_2]]
// CHECK:   br i1 [[EQUAL_TYPES_4_2]], label %[[EXIT_PRESPECIALIZED_4:[0-9]+]], label %[[EXIT_NORMAL:[0-9]+]]
// CHECK: [[EXIT_PRESPECIALIZED_1]]:
// CHECK:   ret %swift.metadata_response { %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVyS2iGMf" to %swift.full_type*), i32 0, i32 1), [[INT]] 0 }
// CHECK: [[EXIT_PRESPECIALIZED_2]]:
// CHECK:     ret %swift.metadata_response { %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVySdSiGMf" to %swift.full_type*), i32 0, i32 1), [[INT]] 0 }
// CHECK: [[EXIT_PRESPECIALIZED_3]]:
// CHECK:     ret %swift.metadata_response { %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVySSSdGMf" to %swift.full_type*), i32 0, i32 1), [[INT]] 0 }
// CHECK: [[EXIT_PRESPECIALIZED_4]]:
// CHECK:     ret %swift.metadata_response { %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main5ValueVys5UInt8VSSGMf" to %swift.full_type*), i32 0, i32 1), [[INT]] 0 }
// CHECK: [[EXIT_NORMAL]]:
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata([[INT]] %0, i8* [[ERASED_TYPE_1]], i8* [[ERASED_TYPE_2]], i8* undef, %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueVMn" to %swift.type_descriptor*)) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
