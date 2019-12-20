// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment --check-prefix=CHECK --check-prefix=CHECK-%target-vendor

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

//         CHECK: @"$s4main9Ancestor1[[UNIQUE_ID_1:[A-Za-z_0-9]+]]CySiGMf" = linkonce_odr hidden 
//   CHECK-apple: global 
// CHECK-unknown: constant 
//              : <{ 
//              :   void (%T4main9Ancestor1[[UNIQUE_ID_1]]C*)*, 
//              :   i8**, 
//              :   i64, 
//              :   %objc_class*, 
//              :   %swift.opaque*, 
//              :   %swift.opaque*, 
//              :   i64, 
//              :   i32, 
//              :   i32, 
//              :   i32, 
//              :   i16, 
//              :   i16, 
//              :   i32, 
//              :   i32, 
//              :   %swift.type_descriptor*, 
//              :   i8*, 
//              :   %swift.type*, 
//              :   i64, 
//              :   %T4main9Ancestor1[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)* 
//              : }> <{ 
//              :   void (%T4main9Ancestor1[[UNIQUE_ID_1]]C*)* 
//              :   @"$s4main9Ancestor1[[UNIQUE_ID_1]]CfD", 
//              :   i8** @"$sBoWV", 
//              :   i64 ptrtoint (
//              :     %objc_class* @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMM" to i64
//              :   ), 
//              :   %objc_class* @"OBJC_CLASS_$__TtCs12_SwiftObject", 
//              :   %swift.opaque* @_objc_empty_cache, 
//              :   %swift.opaque* null, 
//              :   i64 add (
//              :     i64 ptrtoint (
//              :       { 
//              :         i32, 
//              :         i32, 
//              :         i32, 
//              :         i32, 
//              :         i8*, 
//              :         i8*, 
//              :         i8*, 
//              :         i8*, 
//              :         { 
//              :           i32, 
//              :           i32, 
//              :           [1 x { i64*, i8*, i8*, i32, i32 }] 
//              :         }*, 
//              :         i8*, 
//              :         i8* 
//              :       }* @_DATA__TtC4mainP33_496329636AC05466637A72F247DC6ABC9Ancestor1 to i64
//              :     ), 
//              :     i64 2
//              :   ), 
//              :   i32 26, 
//              :   i32 0, 
//              :   i32 16, 
//              :   i16 7, 
//              :   i16 0, 
//              :   i32 120, 
//              :   i32 16, 
//              :   %swift.type_descriptor* bitcast (
//              :     <{ 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i32, 
//              :       i16, 
//              :       i16, 
//              :       i16, 
//              :       i16, 
//              :       i8, 
//              :       i8, 
//              :       i8, 
//              :       i8, 
//              :       i32, 
//              :       i32, 
//              :       %swift.method_descriptor 
//              :     }>* @"$s4main9Ancestor1[[UNIQUE_ID_1]]CMn" 
//              :     to %swift.type_descriptor*
//              :   ), 
//              :   i8* null, 
//              :   %swift.type* @"$sSiN", 
//              :   i64 16, 
//              :   %T4main9Ancestor1[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)* 
//              :   @"$s4main9Ancestor1[[UNIQUE_ID_1]]C5firstADyxGx_tcfC" 
//              : }>, align [[ALIGNMENT]]


//         CHECK: @"$s4main5Value[[UNIQUE_ID_1]]CySiGMf" = linkonce_odr hidden 
//   CHECK-apple: global
// CHECK-unknown: constant
//    CHECK-SAME: <{
//    CHECK-SAME:   void (%T4main5Value[[UNIQUE_ID_1]]C*)*, 
//    CHECK-SAME:   i8**, 
//    CHECK-SAME:   [[INT]], 
//    CHECK-SAME:   %swift.type*, 
//    CHECK-SAME:   %swift.opaque*, 
//    CHECK-SAME:   %swift.opaque*, 
//    CHECK-SAME:   [[INT]], 
//    CHECK-SAME:   i32, 
//    CHECK-SAME:   i32, 
//    CHECK-SAME:   i32, 
//    CHECK-SAME:   i16, 
//    CHECK-SAME:   i16, 
//    CHECK-SAME:   i32, 
//    CHECK-SAME:   i32, 
//    CHECK-SAME:   %swift.type_descriptor*, 
//    CHECK-SAME:   void (%T4main5Value[[UNIQUE_ID_1]]C*)*, 
//    CHECK-SAME:   %swift.type*, 
//    CHECK-SAME:   [[INT]], 
//    CHECK-SAME:   %T4main5Value[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)*, 
//    CHECK-SAME:   %swift.type*, 
//    CHECK-SAME:   [[INT]] 
//    CHECK-SAME: }> <{ 
//    CHECK-SAME:   void (%T4main5Value[[UNIQUE_ID_1]]C*)* @"$s4main5Value[[UNIQUE_ID_1]]CfD", 
//    CHECK-SAME:   i8** @"$sBoWV", 
//   CHECK-apple-SAME:   [[INT]] ptrtoint (
//   CHECK-apple-SAME:     %objc_class* @"$s4main5Value[[UNIQUE_ID_1]]CySiGMM" to [[INT]]
//   CHECK-apple-SAME:   ), 
// CHECK-unknown-SAME:   [[INT]] 0,
//    CHECK-SAME:   %swift.type* getelementptr inbounds (
//    CHECK-SAME:     %swift.full_heapmetadata, 
//    CHECK-SAME:     %swift.full_heapmetadata* bitcast (
//    CHECK-SAME:       <{ 
//    CHECK-SAME:         void (%T4main9Ancestor1[[UNIQUE_ID_1]]C*)*, 
//    CHECK-SAME:         i8**, 
//    CHECK-SAME:         [[INT]], 
//   CHECK-apple-SAME:         %objc_class*, 
// CHECK-unknown-SAME:         %swift.type*, 
//    CHECK-SAME:         %swift.opaque*, 
//    CHECK-SAME:         %swift.opaque*, 
//    CHECK-SAME:         [[INT]], 
//    CHECK-SAME:         i32, 
//    CHECK-SAME:         i32, 
//    CHECK-SAME:         i32, 
//    CHECK-SAME:         i16, 
//    CHECK-SAME:         i16, 
//    CHECK-SAME:         i32, 
//    CHECK-SAME:         i32, 
//    CHECK-SAME:         %swift.type_descriptor*, 
//    CHECK-SAME:         i8*, 
//    CHECK-SAME:         %swift.type*, 
//    CHECK-SAME:         [[INT]], 
//    CHECK-SAME:         %T4main9Ancestor1[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)* 
//    CHECK-SAME:       }>* @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMf" 
//    CHECK-SAME:       to %swift.full_heapmetadata*
//    CHECK-SAME:     ), 
//    CHECK-SAME:     i32 0, 
//    CHECK-SAME:     i32 2
//    CHECK-SAME:   ), 
//   CHECK-apple-SAME:   %swift.opaque* @_objc_empty_cache, 
// CHECK-unknown-SAME:   %swift.opaque* null, 
//    CHECK-SAME:   %swift.opaque* null, 
//   CHECK-apple-SAME:   [[INT]] add (
//   CHECK-apple-SAME:     [[INT]] ptrtoint (
//   CHECK-apple-SAME:       { 
//   CHECK-apple-SAME:         i32, 
//   CHECK-apple-SAME:         i32, 
//   CHECK-apple-SAME:         i32, 
//                   :         i32, 
//   CHECK-apple-SAME:         i8*, 
//   CHECK-apple-SAME:         i8*, 
//   CHECK-apple-SAME:         i8*, 
//   CHECK-apple-SAME:         i8*, 
//   CHECK-apple-SAME:         { 
//   CHECK-apple-SAME:           i32, 
//   CHECK-apple-SAME:           i32, 
//   CHECK-apple-SAME:           [1 x { [[INT]]*, i8*, i8*, i32, i32 }] 
//   CHECK-apple-SAME:         }*, 
//   CHECK-apple-SAME:         i8*, 
//   CHECK-apple-SAME:         i8* 
//   CHECK-apple-SAME:       }* @_DATA__TtC4mainP33_496329636AC05466637A72F247DC6ABC5Value to [[INT]]
//   CHECK-apple-SAME:     ), 
//   CHECK-apple-SAME:     [[INT]] 2
//   CHECK-apple-SAME:   ), 
// CHECK-unknown:   i64 1,
//    CHECK-SAME:   i32 26, 
//    CHECK-SAME:   i32 0, 
//    CHECK-SAME:   i32 {{(32|16)}}, 
//    CHECK-SAME:   i16 {{(7|3)}}, 
//    CHECK-SAME:   i16 0, 
//    CHECK-SAME:   i32 {{(136|80)}}, 
//    CHECK-SAME:   i32 {{(16|8)}}, 
//    CHECK-SAME:   %swift.type_descriptor* bitcast (
//    CHECK-SAME:     <{ 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       i16, 
//    CHECK-SAME:       i16, 
//    CHECK-SAME:       i16, 
//    CHECK-SAME:       i16, 
//    CHECK-SAME:       i8, 
//    CHECK-SAME:       i8, 
//    CHECK-SAME:       i8, 
//    CHECK-SAME:       i8, 
//    CHECK-SAME:       i32, 
//    CHECK-SAME:       %swift.method_override_descriptor 
//    CHECK-SAME:     }>* @"$s4main5Value[[UNIQUE_ID_1]]CMn" 
//    CHECK-SAME:     to %swift.type_descriptor*
//    CHECK-SAME:   ), 
//    CHECK-SAME:   void (%T4main5Value[[UNIQUE_ID_1]]C*)* 
//    CHECK-SAME:   @"$s4main5Value[[UNIQUE_ID_1]]CfE", 
//    CHECK-SAME:   %swift.type* @"$sSiN", 
//    CHECK-SAME:   [[INT]] {{(16|8)}}, 
//    CHECK-SAME:   %T4main5Value[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)* 
//    CHECK-SAME:   @"$s4main5Value[[UNIQUE_ID_1]]C5firstADyxGx_tcfC", 
//    CHECK-SAME:   %swift.type* @"$sSiN", 
//    CHECK-SAME:   [[INT]] {{(24|12)}}
//    CHECK-SAME: }>, align [[ALIGNMENT]]


fileprivate class Ancestor1<First> {
  let first_Ancestor1: First

  init(first: First) {
    self.first_Ancestor1 = first
  }
}

fileprivate class Value<First> : Ancestor1<First> {
  let first_Value: First

  override init(first: First) {
    self.first_Value = first
    super.init(first: first)
  }
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

func doit() {
  consume( Value(first: 13) )
}
doit()

//         CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s4main5Value[[UNIQUE_ID_1]]CySiGMb"
//    CHECK-NEXT: entry:
// CHECK-unknown:   [[SUPER_CLASS_METADATA:%[0-9]+]] = call swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMb"([[INT]] 0)
//   CHECK-apple:  [[THIS_CLASS_METADATA:%[0-9]+]] = call %objc_class* @objc_opt_self(
//   CHECK-apple:    %objc_class* bitcast (
// CHECK-unknown:    ret
//              :     %objc_class* bitcast (
//              :       %swift.type* getelementptr inbounds (
//              :         %swift.full_heapmetadata, 
//              :         %swift.full_heapmetadata* bitcast (
//              :           <{ 
//              :             void (%T4main5Value[[UNIQUE_ID_1]]C*)*, 
//              :             i8**, 
//              :             i64, 
//              :             %swift.type*, 
//              :             %swift.opaque*, 
//              :             %swift.opaque*, 
//              :             i64, 
//              :             i32, 
//              :             i32, 
//              :             i32, 
//              :             i16, 
//              :             i16, 
//              :             i32, 
//              :             i32, 
//              :             %swift.type_descriptor*, 
//              :             void (%T4main5Value[[UNIQUE_ID_1]]C*)*, 
//              :             %swift.type*, 
//              :             i64, 
//              :             %T4main5Value[[UNIQUE_ID_1]]C* (%swift.opaque*, %swift.type*)*, 
//              :             %swift.type*, 
//              :             i64 
//              :           }>* 
//    CHECK-SAME:           @"$s4main5Value[[UNIQUE_ID_1]]CySiGMf" 
//              :           to %swift.full_heapmetadata*
//              :         ), 
//              :         i32 0, 
//              :         i32 2
//              :       ) to %objc_class*
//              :     )
//              :   )
//   CHECK-apple:   [[THIS_TYPE_METADATA:%[0-9]+]] = bitcast %objc_class* [[THIS_CLASS_METADATA]] to %swift.type*
//   CHECK-apple:   [[SUPER_CLASS_METADATA:%[0-9]+]] = call swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMb"([[INT]] 0)
//   CHECK-apple:   [[RESPONSE:%[0-9]+]] = insertvalue %swift.metadata_response undef, %swift.type* [[THIS_TYPE_METADATA]], 0
//   CHECK-apple:   [[COMPLETE_RESPONSE:%[0-9]+]] = insertvalue %swift.metadata_response [[RESPONSE]], [[INT]] 0, 1
//   CHECK-apple:   ret %swift.metadata_response [[COMPLETE_RESPONSE]]
//         CHECK: }

//         CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMb"

