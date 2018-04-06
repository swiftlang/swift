// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_decls -primary-file %s %S/Inputs/coverage_imports.swift | %FileCheck %s

// CHECK: sil_coverage_map {{.*}} $S14coverage_decls4mainyyF
// CHECK-NOT: sil_coverage_map

func main() {
  var b = Box()
  let _ = b.x
}
