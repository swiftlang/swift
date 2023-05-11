// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %use_no_opaque_pointers -I %t -emit-ir  -disable-availability-checking -enable-library-evolution %s | %IRGenFileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir  -disable-availability-checking -enable-library-evolution %s
// REQUIRES: concurrency

// CHECK: @"$s13default_actor1ACMn" = hidden constant
//   0x81800050: 0x01800000 IsActor + IsDefaultActor
//   0x81810050: the same, but using a singleton metadata initialization
// CHECK-SAME: i32 {{-2122317744|-2122252208}},

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1ACfD"(%T13default_actor1AC* swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocate(
// CHECK:     ret void
actor A {}
