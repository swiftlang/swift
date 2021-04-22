// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-concurrency -enable-library-evolution -emit-module-path=%t/resilient_actor.swiftmodule -module-name=resilient_actor %S/Inputs/resilient_actor.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-experimental-concurrency -enable-library-evolution %s | %IRGenFileCheck %s
// REQUIRES: concurrency

// CHECK: @"$s13default_actor1ACMn" = hidden constant
//   0x81000050: 0x01000000 IsDefaultActor
//   0x81010050: the same, but using a singleton metadata initialization
// CHECK-SAME: i32 {{-2130706352|-2130640816}},

// CHECK: @"$s13default_actor1BCMn" = hidden constant
//   0x62010050: 0x02000000 IndirectTypeDescriptor + 0x01000000 IsDefaultActor
// CHECK-SAME: i32 1644232784,

// CHECK: @"$s13default_actor1CCMn" = hidden constant
//   0x62010050: 0x02000000 IndirectTypeDescriptor + 0x01000000 IsDefaultActor
// CHECK-SAME: i32 1644232784,

// CHECK: @"$s13default_actor1DCMn" = hidden constant
//   0x63010050: 0x02000000 IndirectTypeDescriptor + 0x01000000 IsDefaultActor
// CHECK-SAME: i32 1661010000,

import resilient_actor

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1ACfD"(%T13default_actor1AC* swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocate(
// CHECK:     ret void
actor A {}

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1BCfD"(%T13default_actor1BC* swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocateResilient(
// CHECK:     ret void
actor B : ResilientBaseActor {}

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1CCfD"(%T13default_actor1CC* swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocateResilient(
// CHECK:     ret void
actor C : FixedSubclassOfResilientBaseActor {}

// CHECK-LABEL: define hidden swiftcc void @"$s13default_actor1DCfD"(%T13default_actor1DC* swiftself %0)
// CHECK-NOT: ret void
// CHECK:     call swiftcc void @swift_defaultActor_deallocate(
// CHECK:     ret void
actor D : FixedBaseActor {}
