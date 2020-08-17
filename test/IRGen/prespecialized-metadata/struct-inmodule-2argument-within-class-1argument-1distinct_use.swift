// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main9NamespaceC5ValueVySS_SiSdGWV" = linkonce_odr hidden constant %swift.vwtable {
// CHECK-SAME:    i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9_]+" to i8\*|[^@]+@__swift_memcpy[^[:space:]]+ to i8\*)}}),
// CHECK-SAME:    i8* bitcast ({{[^@]*}}@__swift_noop_void_return{{[^[:space:]]* to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@__swift_memcpy{{[^[:space:]]+ to i8\*}}),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@"$s4main9NamespaceC5ValueVySS_SiSdGwet{{[^@]+}} to i8*),
// CHECK-SAME:    i8* bitcast ({{[^@]+}}@"$s4main9NamespaceC5ValueVySS_SiSdGwst{{[^@]+}} to i8*),
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    [[INT]] {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}},
// CHECK-SAME:    i32 {{[0-9]+}}
// CHECK-SAME: },
// NOTE: ignore the COMDAT on PE/COFF platforms
// CHECK-SAME: align [[ALIGNMENT]]

// CHECK: @"$s4main9NamespaceC5ValueVySS_SiSdGMf" = linkonce_odr hidden constant {{.+}}$s4main9NamespaceC5ValueVMn{{.+}} to %swift.type_descriptor*), %swift.type* @"$sSSN", %swift.type* @"$sSiN", %swift.type* @"$sSdN", i32 0, i32 8, i64 3 }>, align [[ALIGNMENT]]
final class Namespace<Arg> {
  struct Value<First, Second> {
    let first: First
    let second: Second
  }
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(%swift.opaque* noalias nocapture %{{[0-9]+}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, %swift.type*, %swift.type*, i32, i32, i64 }>* @"$s4main9NamespaceC5ValueVySS_SiSdGMf" to %swift.full_type*), i32 0, i32 1))
// CHECK: }
func doit() {
  consume( Namespace<String>.Value(first: 13, second: 13.0) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind readnone
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main9NamespaceC5ValueVMa"([[INT]] %0, %swift.type* %1, %swift.type* %2, %swift.type* %3) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_TYPE_1:%[0-9]+]] = bitcast %swift.type* %1 to i8*
// CHECK:   [[ERASED_TYPE_2:%[0-9]+]] = bitcast %swift.type* %2 to i8*
// CHECK:   [[ERASED_TYPE_3:%[0-9]+]] = bitcast %swift.type* %3 to i8*
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata([[INT]] %0, i8* [[ERASED_TYPE_1]], i8* [[ERASED_TYPE_2]], i8* [[ERASED_TYPE_3]], %swift.type_descriptor* bitcast ({{.+}}$s4main9NamespaceC5ValueVMn{{.+}} to %swift.type_descriptor*)) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
