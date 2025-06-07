// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future %S/Inputs/struct-public-nonfrozen-1argument.swift -emit-library -o %t/%target-library-name(Argument) -emit-module -module-name Argument -emit-module-path %t/Argument.swiftmodule
// RUN: %swift -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -L %t -I %t -lArgument | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment 

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

//      CHECK: @"$s8Argument03OneA0Vy4main03TheA0VGMN" = linkonce_odr hidden constant <{
// CHECK-SAME:   ptr,
// CHECK-SAME:   [[INT]],
// CHECK-SAME:   ptr,
// CHECK-SAME:   ptr,
// CHECK-SAME:   i32,
// CHECK-SAME:   i32,
// CHECK-SAME:   i64
// CHECK-SAME: }> <{
//           :   ptr getelementptr inbounds (
//           :     %swift.vwtable,
//           :     
// CHECK-SAME:       $s8Argument03OneA0Vy4main03TheA0VGWV
//           :     ",
//           :     i32 0,
//           :     i32 0
//           :   ),
// CHECK-SAME:   [[INT]] 512,
// CHECK-SAME:   $s8Argument03OneA0VMn
// CHECK-SAME:   $s4main11TheArgumentVMf
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 {{8|4}},
// TRAILING FLAGS: ...01
//                    ^ statically specialized canonical (false)
//                     ^ statically specialized (true)
// CHECK-SAME:   i64 1
// CHECK-SAME: }>,
// CHECK-SAME: align [[ALIGNMENT]]

@inline(never)
func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

import Argument

struct TheArgument {
  let value: Int
}

//      CHECK: define hidden swiftcc void @"$s4main4doityyF"() #{{[0-9]+}} {
//      CHECK:   [[CANONICALIZED_METADATA_RESPONSE:%[0-9]+]] = call swiftcc %swift.metadata_response @swift_getCanonicalSpecializedMetadata(
// CHECK-SAME:     [[INT]] 0, 
// CHECK-SAME:     ptr getelementptr inbounds (
// CHECK-SAME:       %swift.full_type,
// CHECK-SAME:       $s8Argument03OneA0Vy4main03TheA0VGMN
// CHECK-SAME:       i32 0,
// CHECK-SAME:       i32 2
// CHECK-SAME:     ),
// CHECK-SAME:     $s8Argument03OneA0Vy4main03TheA0VGMJ
// CHECK-SAME:   )
// CHECK-NEXT:   [[CANONICALIZED_METADATA:%[0-9]+]] = extractvalue %swift.metadata_response [[CANONICALIZED_METADATA_RESPONSE]], 0
// CHECK-NEXT:   call swiftcc void @"$s4main7consumeyyxlF"(
// CHECK-SAME:     ptr noalias {{%[0-9]+}}, 
// CHECK-SAME:     ptr [[CANONICALIZED_METADATA]]
// CHECK-SAME:   )
//      CHECK: }
func doit() {
  consume( OneArgument(TheArgument(value: 13)) )
}
doit()

