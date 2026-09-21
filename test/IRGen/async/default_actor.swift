// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %t -emit-ir  -target %target-swift-5.1-abi-triple -enable-library-evolution %s | %IRGenFileCheck %s
// REQUIRES: concurrency

// CHECK: @"$s13default_actor1ACMn" = hidden constant
//   0x81880050: 0x01800000 IsActor + IsDefaultActor + HasSingletonMetadataPointer
//   0x81810050: singleton metadata initialization instead, which precludes the
//               singleton metadata pointer
// CHECK-SAME: i32 {{-2121793456|-2122252208}},

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1ACfD"(ptr swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocate(
// CHECK:     ret void
actor A {}
