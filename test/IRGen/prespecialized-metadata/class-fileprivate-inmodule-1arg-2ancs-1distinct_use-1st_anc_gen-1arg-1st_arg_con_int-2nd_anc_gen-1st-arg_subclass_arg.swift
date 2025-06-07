// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment --check-prefix=CHECK --check-prefix=CHECK-%target-vendor

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK-DAG: @"$s4main9Ancestor1[[UNIQUE_ID_1:[0-9a-zA-Z_]+]]CySiGMf" =
// CHECK-DAG: @"$s4main9Ancestor2[[UNIQUE_ID_1]]CySiGMf" = 

//              CHECK: @"$s4main5Value[[UNIQUE_ID_1]]CySSGMf" = linkonce_odr hidden 
// CHECK-unknown-SAME: constant 
//   CHECK-apple-SAME: global 
//         CHECK-SAME: <{
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   ptr,
//                   :   [[INT]],
//         CHECK-SAME:   ptr,
//   CHECK-apple-SAME:   ptr,
//   CHECK-apple-SAME:   ptr,
//   CHECK-apple-SAME:   [[INT]],
//         CHECK-SAME:   i32,
//         CHECK-SAME:   i32,
//         CHECK-SAME:   i32,
//         CHECK-SAME:   i16,
//         CHECK-SAME:   i16,
//         CHECK-SAME:   i32,
//         CHECK-SAME:   i32,
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   [[INT]],
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   [[INT]],
//         CHECK-SAME:   ptr,
//         CHECK-SAME:   [[INT]],
//         CHECK-SAME:   ptr
//         CHECK-SAME: }> <{
//         CHECK-SAME:   $s4main5Value[[UNIQUE_ID_1]]CfD
//         CHECK-SAME:   $sBoWV
//   CHECK-apple-SAME:   $s4main5Value[[UNIQUE_ID_1]]CySSGMM
// CHECK-unknown-SAME:   [[INT]] 0,
//                   :   ptr getelementptr inbounds (
//                   :     %swift.full_heapmetadata,
//                   :     $s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMf
//                   :     i32 0,
//                   :     i32 2
//                   :   ),
//   CHECK-apple-SAME:   _objc_empty_cache
//   CHECK-apple-SAME:   ptr null,
//   CHECK-apple-SAME:   [[INT]] add (
//   CHECK-apple-SAME:     [[INT]] ptrtoint (
//   CHECK-apple-SAME:       ptr {{[^@]*}}@"_DATA_$s4main5Value[[UNIQUE_ID_1]]CySSGMf" to [[INT]]
//   CHECK-apple-SAME:     ),
//   CHECK-apple-SAME:     [[INT]] 2
//   CHECK-apple-SAME:   ),
//         CHECK-SAME:   i32 26,
//         CHECK-SAME:   i32 0,
//         CHECK-SAME:   i32 {{(48|28)}},
//         CHECK-SAME:   i16 {{(7|3)}},
//         CHECK-SAME:   i16 0,
//   CHECK-apple-SAME:   i32 {{(168|96)}},
// CHECK-unknown-SAME:   i32 144,
//         CHECK-SAME:   i32 {{(24|12)}},
//                   :   $s4main5Value[[UNIQUE_ID_1]]CMn
//         CHECK-SAME:   $s4main5Value[[UNIQUE_ID_1]]CfE
//         CHECK-SAME:   $sSSN
//         CHECK-SAME:   [[INT]] {{(16|8)}},
//         CHECK-SAME:   $s4main5Value[[UNIQUE_ID_1]]C5firstADyxGSi_tcfCAA9Ancestor2ACLLCAeHyxGx_tcfCTV
//         CHECK-SAME:   $sSSN
//         CHECK-SAME:   [[INT]] {{(24|12)}},
//         CHECK-SAME:   $sSSN
//         CHECK-SAME:   [[INT]] {{(32|16)}},
//         CHECK-SAME:   $s4main5Value[[UNIQUE_ID_1]]C5firstADyxGx_tcfC
//         CHECK-SAME: }>,
//         CHECK-SAME: align [[ALIGNMENT]]

fileprivate class Ancestor2<First> {
  let first_Ancestor2: First

  init(first: First) {
    self.first_Ancestor2 = first
  }
}

fileprivate class Ancestor1<First> : Ancestor2<First> {
  let first_Ancestor1: First

  override init(first: First) {
    self.first_Ancestor1 = first
    super.init(first: first)
  }
}

fileprivate class Value<First> : Ancestor1<Int> {
  let first_Value: First

  init(first: First) {
    self.first_Value = first
    super.init(first: 13)
  }
}

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

// CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
// CHECK:   [[METADATA_RESPONSE:%[0-9]+]] = call swiftcc %swift.metadata_response @"$s4main5Value[[UNIQUE_ID_1]]CySSGMb"([[INT]] 0)
// CHECK:   [[METADATA:%[0-9]+]] = extractvalue %swift.metadata_response [[METADATA_RESPONSE]], 0
// CHECK:   call swiftcc void @"$s4main7consumeyyxlF"(
// CHECK-SAME:     ptr noalias {{%[0-9]+}}, 
// CHECK-SAME:     ptr [[METADATA]])
// CHECK: }
func doit() {
  consume( Value(first: "13") )
}
doit()

//         CHECK: ; Function Attrs: noinline nounwind memory(none)
//         CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s4main5Value[[UNIQUE_ID_1]]CySSGMb"([[INT]] {{%[0-9]+}}) #{{[0-9]+}} {{(section)?.*}}{
//         CHECK: entry:
//         CHECK:   call swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMb"([[INT]] 0)
//     CHECK-NOT:   call swiftcc %swift.metadata_response @"$s4main9Ancestor2[[UNIQUE_ID_1]]CySiGMb"([[INT]] 0)
// CHECK-unknown:   ret
//   CHECK-apple:  [[INITIALIZED_CLASS:%[0-9]+]] = call ptr @objc_opt_self(
//    CHECK-SAME:        @"$s4main5Value[[UNIQUE_ID_1]]CySSGMf" 
//   CHECK-apple:   [[PARTIAL_METADATA_RESPONSE:%[0-9]+]] = insertvalue %swift.metadata_response undef, ptr [[INITIALIZED_CLASS]], 0
//   CHECK-apple:   [[METADATA_RESPONSE:%[0-9]+]] = insertvalue %swift.metadata_response [[PARTIAL_METADATA_RESPONSE]], [[INT]] 0, 1
//   CHECK-apple:   ret %swift.metadata_response [[METADATA_RESPONSE]]
//         CHECK: }

//      CHECK: define internal swiftcc %swift.metadata_response @"$s4main9Ancestor2[[UNIQUE_ID_1]]CMa"([[INT]] [[METADATA_REQUEST:%[0-9]+]], ptr %1) #{{[0-9]+}} {{(section)?.*}}{
//      CHECK: entry:
//      CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateCanonicalPrespecializedGenericMetadata(
// CHECK-SAME:     [[INT]] [[METADATA_REQUEST]], 
// CHECK-SAME:     ptr %1, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     $s4main9Ancestor2[[UNIQUE_ID_1]]CMn
// CHECK-SAME:   ) #{{[0-9]+}}
//      CHECK:   ret %swift.metadata_response {{%[0-9]+}}
//      CHECK: }

//      CHECK: define internal swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CMa"([[INT]] [[METADATA_REQUEST:%[0-9]+]], ptr %1) #{{[0-9]+}} {{(section)?.*}}{
//      CHECK: entry:
//      CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateCanonicalPrespecializedGenericMetadata(
// CHECK-SAME:     [[INT]] [[METADATA_REQUEST]], 
// CHECK-SAME:     ptr %1, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     $s4main9Ancestor1[[UNIQUE_ID_1]]CMn
// CHECK-SAME:   )
//      CHECK:   ret %swift.metadata_response {{%[0-9]+}}
//      CHECK: }

//      CHECK: define internal swiftcc %swift.metadata_response @"$s4main5Value[[UNIQUE_ID_1]]CMa"([[INT]] [[METADATA_REQUEST:%[0-9]+]], ptr %1) #{{[0-9]+}} {{(section)?.*}}{
//      CHECK: entry:
//      CHECK:   {{%[0-9]+}} = call swiftcc %swift.metadata_response @__swift_instantiateCanonicalPrespecializedGenericMetadata(
// CHECK-SAME:     [[INT]] [[METADATA_REQUEST]], 
// CHECK-SAME:     ptr %1, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     ptr undef, 
// CHECK-SAME:     $s4main5Value[[UNIQUE_ID_1]]CMn
// CHECK-SAME:   )
//      CHECK:   ret %swift.metadata_response {{%[0-9]+}}
//      CHECK: }

// CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s4main9Ancestor1[[UNIQUE_ID_1]]CySiGMb"([[INT]] {{%[0-9]+}})

// CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s4main9Ancestor2[[UNIQUE_ID_1]]CySiGMb"([[INT]] {{%[0-9]+}})

