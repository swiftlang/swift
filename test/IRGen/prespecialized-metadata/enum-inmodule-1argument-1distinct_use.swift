// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main5ValueOySiGWV" = linkonce_odr hidden constant %swift.enum_vwtable { 
// CHECK-SAME:   i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9_]+" to i8\*|[^@]+@__swift_memcpy[0-9]+_[0-9]+[^\)]* to i8\*)}}), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_noop_void_return{{[^\)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOySiGwet{{[^)]+}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOySiGwst{{[^)]+}} to i8*), 
// CHECK-SAME:   [[INT]] [[ALIGNMENT]], 
// CHECK-SAME:   [[INT]] [[ALIGNMENT]], 
// CHECK-SAME:   i32 {{[0-9]+}}, 
// CHECK-SAME:   i32 0, 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOySiGwug{{[^)]+}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOySiGwup{{[^)]+}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOySiGwui{{[^)]+}} to i8*) 
// CHECK-SAME: }, align [[ALIGNMENT]]
// CHECK: @"$s4main5ValueOySiGMf" = linkonce_odr hidden constant <{ 
// CHECK-SAME:   i8**, 
// CHECK-SAME:   [[INT]], 
// CHECK-SAME:   %swift.type_descriptor*, 
// CHECK-SAME:   %swift.type*, 
// CHECK-SAME:   i64 
// CHECK-SAME: }> <{ 
// CHECK-SAME: i8** getelementptr inbounds (%swift.enum_vwtable, %swift.enum_vwtable* @"$s4main5ValueOySiGWV", i32 0, i32 0), 
// CHECK-SAME: [[INT]] 513, 
// CHECK-SAME:   %swift.type_descriptor* bitcast (
// CHECK-SAME:     {{.*}}$s4main5ValueOMn{{.*}} to %swift.type_descriptor*
// CHECK-SAME:   ), 
// CHECK-SAME:   %swift.type* @"$sSiN", 
// CHECK-SAME:   i64 3 
// CHECK-SAME: }>, align [[ALIGNMENT]]
enum Value<First> {
  case only(First)
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(
// CHECK-SAME:   %swift.opaque* noalias nocapture %{{[0-9]+}}, 
// CHECK-SAME:   %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* bitcast (<{ i8**, [[INT]], %swift.type_descriptor*, %swift.type*, i64 }>* @"$s4main5ValueOySiGMf" to %swift.full_type*), i32 0, i32 1)
// CHECK-SAME: )
// CHECK: }
func doit() {
  consume( Value.only(13) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind readnone
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueOMa"([[INT]] %0, %swift.type* %1) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_TYPE:%[0-9]+]] = bitcast %swift.type* %1 to i8*
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateGenericMetadata(
// CHECK-SAME:   [[INT]] %0, 
// CHECK-SAME:   i8* %2, 
// CHECK-SAME:   i8* undef, 
// CHECK-SAME:   i8* undef, 
// CHECK-SAME:   %swift.type_descriptor* bitcast (
// CHECK-SAME:     {{.*}}$s4main5ValueOMn{{.*}} to %swift.type_descriptor*
// CHECK-SAME:   )
// CHECK-SAME: ) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
