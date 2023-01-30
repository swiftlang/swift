// RUN: %target-swift-frontend -emit-sil  -profile-generate -profile-coverage-mapping -module-name coverage_deadcode %s | %FileCheck %s -check-prefix SIL
// RUN: %target-swift-frontend -emit-ir -profile-generate -profile-coverage-mapping -module-name coverage_deadcode %s | %FileCheck %s -check-prefix IR

// This function needs to be present in the SIL for the mandatory passes, but
// we can drop it in IRGen. We still need to emit its coverage map though.
func unused() -> Int { 5 }

// SIL: sil hidden @$s17coverage_deadcode6unusedSiyF : $@convention(thin) () -> Int
// SIL: sil_coverage_map {{.*}} "$s17coverage_deadcode6unusedSiyF"

// IR: @__covrec
// IR: @__llvm_coverage_mapping
// IR: @__llvm_prf_nm
// IR-NOT: define {{.*}} @"$s17coverage_deadcode6unusedSiyF"
