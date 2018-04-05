// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -primary-file %s %S/Inputs/empty-func.swift -emit-sil -emit-sorted-sil -Xllvm -sil-full-demangle -module-name multi | %FileCheck %s -check-prefix=PRIMARY
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping %s %S/Inputs/empty-func.swift -emit-sil -emit-sorted-sil -Xllvm -sil-full-demangle -module-name multi | %FileCheck %s -check-prefix=WMO
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -primary-file %s -primary-file %S/Inputs/empty-func.swift -emit-sil -emit-sorted-sil -Xllvm -sil-full-demangle -module-name multi | %FileCheck %s -check-prefix=BOTH-PRIMARY

// PRIMARY-NOT: sil_coverage_map {{.*}} $S5multi10empty_funcyyF
// PRIMARY: sil_coverage_map {{.*}} $S5multi4mainyyF

// WMO: sil_coverage_map {{.*}} $S5multi10empty_funcyyF
// WMO: sil_coverage_map {{.*}} $S5multi4mainyyF

// BOTH-PRIMARY: sil_coverage_map {{.*}} $S5multi10empty_funcyyF
// BOTH-PRIMARY: sil_coverage_map {{.*}} $S5multi4mainyyF

func main() {
  empty_func()
}
