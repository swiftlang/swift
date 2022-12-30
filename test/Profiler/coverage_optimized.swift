// RUN: %target-swift-frontend -emit-irgen  -profile-generate -profile-coverage-mapping -O -module-name coverage_optimized %s | %FileCheck %s -check-prefix IRGEN
// RUN: %target-swift-frontend -emit-ir -profile-generate -profile-coverage-mapping -O -module-name coverage_optimized %s | %FileCheck %s -check-prefix IR

// The functions 'unused' and 'optimizedOut' below will be optimized out, but
// make sure we still emit coverage records for them, using name data emitted
// separately in @__llvm_coverage_names.

// IRGEN: @__llvm_coverage_names {{.*}} @"__profn_{{.*}}$s18coverage_optimized6unusedyyF", {{.*}} @"__profn_{{.*}}$s18coverage_optimized0B3OutSiyF"

// IRGEN: @__covrec
// IRGEN: @__covrec
// IRGEN: @__covrec
// IRGEN: @__covrec
// IRGEN: @__covrec

// IRGEN: @__llvm_coverage_mapping

// IR: @__covrec
// IR: @__covrec
// IR: @__covrec
// IR: @__covrec
// IR: @__covrec

// We shouldn't have any lingering @__profn's, they should have been emitted as
// @__llvm_prf_nm.
// IR-NOT: __profn_
// IR-NOT: @__llvm_coverage_names
// IR: @__llvm_prf_nm
// IR-NOT: __profn_
// IR-NOT: @__llvm_coverage_names

// IR-NOT: define {{.*}} @"$s18coverage_optimized6unusedyyF"
// IR-NOT: define {{.*}} @"$s18coverage_optimized0B3OutSiyF"

func unused() {}
func optimizedOut() -> Int { .random() ? 1 : 2 }

func bar() -> Bool { false }

func baz() {
  if bar() {
    _ = optimizedOut()
  }
}

baz()

// IRGEN-LABEL: define {{.*}} @main
// IRGEN:       call void @llvm.instrprof.increment({{.*}} @"__profn_{{.*}}main
// IRGEN:       call void @llvm.instrprof.increment({{.*}} @"__profn_{{.*}}$s18coverage_optimized3bazyyF"
// IRGEN:       call void @llvm.instrprof.increment({{.*}} @"__profn_{{.*}}$s18coverage_optimized3barSbyF"
