// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// CHECK: [[EXTRA_DATA_PATTERN:@[0-9]+]] = internal constant <{ i64 }> zeroinitializer, align [[ALIGNMENT]]

//      CHECK: @"$s4main6EitherOMP" = internal constant <{ 
//           :   i32, 
//           :   i32, 
//           :   i32, 
//           :   i32, 
//           :   i32, 
//           :   i16, 
//           :   i16 
//           : }> <{
//           :   i32 trunc (
//           :     i64 sub (
//           :       i64 ptrtoint (
//           :         ptr (
//           :           ptr, 
//           :           ptr, 
//           :           ptr
//           :         )* @"$s4main6EitherOMi" to i64
//           :       ), 
//           :       i64 ptrtoint (
//           :         <{ i32, i32, i32, i32, i32, i16, i16 }>* @"$s4main6EitherOMP" to i64
//           :       )
//           :     ) to i32
//           :   ), 
//           :   i32 trunc (
//           :     i64 sub (
//           :       i64 ptrtoint (
//           :         %swift.metadata_response (
//           :           ptr, 
//           :           ptr, 
//           :           ptr
//           :         )* @"$s4main6EitherOMr" to i64
//           :       ), 
//           :       i64 ptrtoint (
//           :         i32* getelementptr inbounds (
//           :           <{ i32, i32, i32, i32, i32, i16, i16 }>, 
//           :           <{ i32, i32, i32, i32, i32, i16, i16 }>* @"$s4main6EitherOMP", 
//           :           i32 0, 
//           :           i32 1
//           :         ) to i64
//           :       )
//           :     ) to i32
//           :   ), 
//           :   i32 1075838979, 
//           :   i32 trunc (
//           :     i64 sub (
//           :       i64 ptrtoint (
//           :         ptr @"$s4main6EitherOWV" to i64
//           :       ), 
//           :       i64 ptrtoint (
//           :         i32* getelementptr inbounds (
//           :           <{ i32, i32, i32, i32, i32, i16, i16 }>, 
//           :           <{ i32, i32, i32, i32, i32, i16, i16 }>* @"$s4main6EitherOMP", 
//           :           i32 0, 
//           :           i32 3
//           :         ) to i64
//           :       )
//           :     ) to i32
//           :   ), 
//           :   i32 trunc (
// CHECK-SAME:     [[INT]] sub (
// CHECK-SAME:       [[INT]] ptrtoint (
// CHECK-SAME:         ptr [[EXTRA_DATA_PATTERN]] to [[INT]]
// CHECK-SAME:       ), 
// CHECK-SAME:       [[INT]] ptrtoint (
// CHECK-SAME:         ptr getelementptr inbounds (
// CHECK-SAME:           <{ i32, i32, i32, i32, i32, i16, i16 }>, 
// CHECK-SAME:           $s4main6EitherOMP
// CHECK-SAME:           i32 0, 
// CHECK-SAME:           i32 4
// CHECK-SAME:         ) to [[INT]]
// CHECK-SAME:       )
// CHECK-SAME:     )
//           :   ), 
//           :   i16 4, 
//           :   i16 1 
//           : }>, align 8

enum Either<First, Second, Third> {
  case first(Int)
  case second(String)
}

