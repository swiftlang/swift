// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %s -emit-singleton-metadata-pointer | %FileCheck %s -DINT=i%target-ptrsize -DALIGNMENT=%target-alignment --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-%target-vendor --dump-input=always

// CHECK: @"$s4main23PrivateNongenericStructVMn" =
// CHECK-SAME: hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32 }>
// -- flags: struct, unique, has singleton metadata pointer
// CHECK-SAME: <{ i32 524369,
// -- 32-bit relative pointer to metadata
// CHECK-64-SAME: i32 trunc (i64 sub (i64 ptrtoint (ptr getelementptr inbounds (<{ {{.*}} }>, ptr @"$s4main23PrivateNongenericStructVMf", i32 0, i32 2) to i64)
// CHECK-32-SAME: i32 sub (i32 ptrtoint (ptr getelementptr inbounds (<{ {{.*}} }>, ptr @"$s4main23PrivateNongenericStructVMf", i32 0, i32 2) to i32)

internal struct PrivateNongenericStruct {}
