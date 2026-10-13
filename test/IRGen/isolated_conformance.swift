// RUN: %target-swift-frontend -primary-file %s -emit-ir -swift-version 6 | %IRGenFileCheck %s

// REQUIRES: PTRSIZE=64
// REQUIRES: concurrency

protocol P {
  func f()
}

// CHECK-arm64e: @"got.$sScMs11GlobalActorsMc" = private unnamed_addr constant ptr @"$sScMs11GlobalActorsMc.ptrauth"
// CHECK-arm64e: @"$sScMs11GlobalActorsMc.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$sScMs11GlobalActorsMc", i32 2, i64 ptrtoint (ptr @"got.$sScMs11GlobalActorsMc" to i64), i64 50923 }, section "llvm.ptrauth"

// CHECK-LABEL: @"$s20isolated_conformance1XVyxGAA1PAAMc" =
// CHECK-SAME: ptr @"$s20isolated_conformance1PMp"
// CHECK-SAME: ptr @"$s20isolated_conformance1XVMn"
// CHECK-SAME: ptr @"$s20isolated_conformance1XVyxGAA1PAAWP
// CHECK-SAME: i32 524288
// CHECK-SAME: ScM
// CHECK-SAME: $sScMs11GlobalActorsMc"
@MainActor
struct X<T>: @MainActor P {
  func f() { }
}
