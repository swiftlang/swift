// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: @"$s4main5ValueOyS4iGWV" = linkonce_odr hidden constant %swift.enum_vwtable { 
// CHECK-SAME:   i8* bitcast ({{(%swift.opaque\* \(\[[0-9]+ x i8\]\*, \[[0-9]+ x i8\]\*, %swift.type\*\)\* @"\$[a-zA-Z0-9_]+" to i8\*|i8\* \(i8\*, i8\*, %swift.type\*\)\* @__swift_memcpy[0-9]+_[0-9]+ to i8\*)}}), 
// CHECK-SAME:   i8* bitcast (void (i8*, %swift.type*)* @__swift_noop_void_return to i8*), 
// CHECK-SAME:   i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*), 
// CHECK-SAME:   i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*), 
// CHECK-SAME:   i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*), 
// CHECK-SAME:   i8* bitcast (i8* (i8*, i8*, %swift.type*)* @__swift_memcpy{{[0-9]+}}_{{[0-9]+}} to i8*), 
// CHECK-SAME:   i8* bitcast (i32 (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueOyS4iGwet" to i8*), 
// CHECK-SAME:   i8* bitcast (void (%swift.opaque*, i32, i32, %swift.type*)* @"$s4main5ValueOyS4iGwst" to i8*), 
// CHECK-SAME:   [[INT]] {{[0-9]+}}, 
// CHECK-SAME:   [[INT]] {{[0-9]+}}, 
// CHECK-SAME:   i32 {{[0-9]+}}, 
// CHECK-SAME:   i32 {{[0-9]+}}, 
// CHECK-SAME:   i8* bitcast (i32 (%swift.opaque*, %swift.type*)* @"$s4main5ValueOyS4iGwug" to i8*), 
// CHECK-SAME:   i8* bitcast (void (%swift.opaque*, %swift.type*)* @"$s4main5ValueOyS4iGwup" to i8*), 
// CHECK-SAME    i8* bitcast (void (%swift.opaque*, i32, %swift.type*)* @"$s4main5ValueOyS4iGwui" to i8*) 
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
// CHECK-SAME:   %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueOMn" to %swift.type_descriptor*), 
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
// CHECK:   br label %[[TYPE_COMPARISON_1:[0-9]+]]
// CHECK: [[TYPE_COMPARISON_1]]:
// CHECK:   [[ERASED_TYPE_ADDRESS_1:%[0-9]+]] = getelementptr i8*, i8** %1, i64 0
// CHECK:   [[ERASED_TYPE_1:%\".*\"]] = load i8*, i8** [[ERASED_TYPE_ADDRESS_1]]
// CHECK:   [[EQUAL_TYPE_1_1:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_1]]
// CHECK:   [[EQUAL_TYPES_1_1:%[0-9]+]] = and i1 true, [[EQUAL_TYPE_1_1]]
// CHECK:   [[ERASED_TYPE_ADDRESS_2:%[0-9]+]] = getelementptr i8*, i8** %1, i64 1
// CHECK:   [[ERASED_TYPE_2:%\".*\"]] = load i8*, i8** [[ERASED_TYPE_ADDRESS_2]]
// CHECK:   [[EQUAL_TYPE_1_2:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_2]]
// CHECK:   [[EQUAL_TYPES_1_2:%[0-9]+]] = and i1 [[EQUAL_TYPES_1_1]], [[EQUAL_TYPE_1_2]]
// CHECK:   [[ERASED_TYPE_ADDRESS_3:%[0-9]+]] = getelementptr i8*, i8** %1, i64 2
// CHECK:   [[ERASED_TYPE_3:%\".*\"]] = load i8*, i8** [[ERASED_TYPE_ADDRESS_3]]
// CHECK:   [[EQUAL_TYPE_1_3:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_3]]
// CHECK:   [[EQUAL_TYPES_1_3:%[0-9]+]] = and i1 [[EQUAL_TYPES_1_2]], [[EQUAL_TYPE_1_3]]
// CHECK:   [[ERASED_TYPE_ADDRESS_4:%[0-9]+]] = getelementptr i8*, i8** %1, i64 3
// CHECK:   [[ERASED_TYPE_4:%\".*\"]] = load i8*, i8** [[ERASED_TYPE_ADDRESS_4]]
// CHECK:   [[EQUAL_TYPE_1_4:%[0-9]+]] = icmp eq i8* bitcast (%swift.type* @"$sSiN" to i8*), [[ERASED_TYPE_4]]
// CHECK:   [[EQUAL_TYPES_1_4:%[0-9]+]] = and i1 [[EQUAL_TYPES_1_3]], [[EQUAL_TYPE_1_4]]
// CHECK:   br i1 [[EQUAL_TYPES_1_4]], label %[[EXIT_PRESPECIALIZED_1:[0-9]+]], label %[[EXIT_NORMAL:[0-9]+]]
// CHECK: [[EXIT_PRESPECIALIZED_1]]:
// CHECK:   ret %swift.metadata_response { 
// CHECK-SAME:     %swift.type* getelementptr inbounds (
// CHECK-SAME:       %swift.full_type, 
// CHECK-SAME:       %swift.full_type* bitcast (
// CHECK-SAME:         <{ 
// CHECK-SAME:           i8**, 
// CHECK-SAME:           [[INT]], 
// CHECK-SAME:           %swift.type_descriptor*, 
// CHECK-SAME:           %swift.type*, 
// CHECK-SAME:           %swift.type*, 
// CHECK-SAME:           %swift.type*, 
// CHECK-SAME:           %swift.type*, 
// CHECK-SAME:           [[INT]], 
// CHECK-SAME:           i64 
// CHECK-SAME:         }>* @"$s4main5ValueOyS4iGMf" 
// CHECK-SAME:         to %swift.full_type*
// CHECK-SAME:       ), 
// CHECK-SAME:       i32 0, 
// CHECK-SAME:       i32 1
// CHECK-SAME:     ), 
// CHECK-SAME:     [[INT]] 0 
// CHECK-SAME:   }
// CHECK: [[EXIT_NORMAL]]:
// CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @swift_getGenericMetadata(
// CHECK-SAME: [[INT]] %0, 
// CHECK-SAME: i8* [[ERASED_BUFFER]], 
// CHECK-SAME: %swift.type_descriptor* bitcast (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i16, i16, i16, i16, i8, i8, i8, i8 }>* @"$s4main5ValueOMn" to %swift.type_descriptor*)
// CHECK-SAME: ) #{{[0-9]+}}
// CHECK:   ret %swift.metadata_response {{%[0-9]+}}
// CHECK: }
