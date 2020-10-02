// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main5ValueVyS2iGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9]+" to i8\*|i8\* \(i8\*, i8\*, %swift.type\*\)\* @__swift_memcpy[0-9]+_[0-9]+ to i8\*|{ i8\*, i32, i64, i64 }\* @__swift_memcpy[0-9]+_[0-9]+.ptrauth to i8\*)}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_noop_void_return{{[^[:space:]]* to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@"$s4main5ValueVyS2iGwet{{[^[:space:]]* to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@"$s4main5ValueVyS2iGwst{{[^[:space:]]* to i8\*}}),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore COMDAT on PE/COFF targets
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main5ValueVyS2iGMf" = linkonce_odr hidden constant {{.+}}$s4main5ValueVMn{{.+}} to %swift.type_descriptor*), %swift.type* @"$sSiN", %swift.type* @"$sSiN", i32 0, i32 [[ALIGNMENT]], i64 3 }>, align [[ALIGNMENT]]
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
// CHECK: }
func doit() {
  consume( Value(first: 13, second: 13) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind readnone
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueVMa"([[INT]] %0, %swift.type* %1, %swift.type* %2) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_TYPE_1:%[0-9]+]] = bitcast %swift.type* %1 to i8*
// CHECK:   [[ERASED_TYPE_2:%[0-9]+]] = bitcast %swift.type* %2 to i8*
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata([[INT]] %0, i8* [[ERASED_TYPE_1]], i8* [[ERASED_TYPE_2]], i8* undef, %swift.type_descriptor* bitcast ({{.+}}$s4main5ValueVMn{{.+}} to %swift.type_descriptor*)) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
