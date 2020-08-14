// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main5ValueOyS4iGWV" = linkonce_odr hidden constant %swift.enum_vwtable { 
// CHECK-SAME:   i8* bitcast ({{([^@]+@"\$s4main5ValueOyS4iGwCP+[^\)]+ to i8\*|[^@]+@__swift_memcpy[0-9]+_[0-9]+[^\)]+ to i8\*)}}), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_noop_void_return{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@__swift_memcpy{{[0-9]+}}_{{[0-9]+}}{{[^)]*}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOyS4iGwet{{[^)]+}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOyS4iGwst{{[^)]+}} to i8*), 
// CHECK-SAME:   [[INT]] {{[0-9]+}}, 
// CHECK-SAME:   [[INT]] {{[0-9]+}}, 
// CHECK-SAME:   i32 {{[0-9]+}}, 
// CHECK-SAME:   i32 {{[0-9]+}}, 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOyS4iGwug{{[^)]+}} to i8*), 
// CHECK-SAME:   i8* bitcast ({{[^@]+}}@"$s4main5ValueOyS4iGwup{{[^)]+}} to i8*), 
// CHECK-SAME    i8* bitcast ({{[^@]+}}@"$s4main5ValueOyS4iGwui{{[^)]+}} to i8*) 
// CHECK-SAME: }, align [[ALIGNMENT]]
// CHECK: @"$s4main5ValueOyS4iGMf" = linkonce_odr hidden constant <{ 
// CHECK-SAME:   i8**, 
// CHECK-SAME:   [[INT]], 
// CHECK-SAME:   %swift.type_descriptor*, 
// CHECK-SAME:   %swift.type*, 
// CHECK-SAME:   i64 
// CHECK-SAME:   }> <{ 
// CHECK-SAME:   i8** getelementptr inbounds (%swift.enum_vwtable, %swift.enum_vwtable* @"$s4main5ValueOyS4iGWV", i32 0, i32 0), 
// CHECK-SAME:   [[INT]] 513, 
// CHECK-SAME:   %swift.type_descriptor* bitcast (
// CHECK-SAME:     {{.*}}$s4main5ValueOMn{{.*}} to %swift.type_descriptor*
// CHECK-SAME:   ), 
// CHECK-SAME:   %swift.type* @"$sSiN", 
// CHECK-SAME:   %swift.type* @"$sSiN", 
// CHECK-SAME:   %swift.type* @"$sSiN", 
// CHECK-SAME:   %swift.type* @"$sSiN", 
// CHECK-SAME:   [[INT]] {{32|16}},
// CHECK-SAME:   i64 3 
// CHECK-SAME: }>, align [[ALIGNMENT]]
enum Value<First, Second, Third, Fourth> {
  case first(First)
  case second(First, Second)
  case third(First, Second, Third)
  case fourth(First, Second, Third, Fourth)
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(
// CHECK-SAME:   %swift.opaque* noalias nocapture {{%[0-9]+}}, 
// CHECK-SAME:   %swift.type* getelementptr inbounds (
// CHECK-SAME:     %swift.full_type, 
// CHECK-SAME:     %swift.full_type* bitcast (
// CHECK-SAME:       <{ 
// CHECK-SAME:         i8**, 
// CHECK-SAME:         [[INT]], 
// CHECK-SAME:         %swift.type_descriptor*, 
// CHECK-SAME:         %swift.type*, 
// CHECK-SAME:         %swift.type*, 
// CHECK-SAME:         %swift.type*, 
// CHECK-SAME:         %swift.type*, 
// CHECK-SAME:         [[INT]], 
// CHECK-SAME:         i64 
// CHECK-SAME:       }>* @"$s4main5ValueOyS4iGMf" 
// CHECK-SAME:       to %swift.full_type*
// CHECK-SAME:     ), 
// CHECK-SAME:     i32 0, 
// CHECK-SAME:     i32 1
// CHECK-SAME:   )
// CHECK-SAME: )
// CHECK: }
func doit() {
  consume( Value.fourth(13, 14, 15, 16) )
}
doit()

// CHECK: ; Function Attrs: noinline nounwind
// CHECK: define hidden swiftcc %swift.metadata_response @"$s4main5ValueOMa"([[INT]] %0, i8** %1) #{{[0-9]+}} {
// CHECK: entry:
// CHECK:   [[ERASED_BUFFER:%[0-9]+]] = bitcast i8** %1 to i8*
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @swift_getGenericMetadata(
// CHECK-SAME:     [[INT]] %0, 
// CHECK-SAME:     i8* [[ERASED_BUFFER]], 
// CHECK-SAME:     %swift.type_descriptor* bitcast (
// CHECK-SAME:       {{.*}}$s4main5ValueOMn{{.*}} to %swift.type_descriptor*
// CHECK-SAME:     )
// CHECK-SAME:   ) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
