// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_primary_file %s %S/Inputs/coverage_imports.swift | %FileCheck %s -check-prefix=ALL
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_primary_file -primary-file %s %S/Inputs/coverage_imports.swift | %FileCheck %s -check-prefix=PRIMARY

// ALL: sil_coverage_map {{.*}} // closure #1 () -> Swift.Int in coverage_primary_file.Box.x.getter : Swift.Int
// ALL: sil_coverage_map {{.*}} // coverage_primary_file.Box.init(y: Swift.Int) -> coverage_primary_file.Box
// ALL: sil_coverage_map {{.*}} // coverage_primary_file.Box.init(z: Swift.String) -> coverage_primary_file.Box
// ALL: sil_coverage_map {{.*}} // coverage_primary_file.main() -> ()
// ALL: sil_coverage_map {{.*}} // __ntd_Box

// PRIMARY-NOT: sil_coverage_map
// PRIMARY: sil_coverage_map {{.*}} // coverage_primary_file.Box.init(y: Swift.Int) -> coverage_primary_file.Box
// PRIMARY: sil_coverage_map {{.*}} // coverage_primary_file.Box.init(z: Swift.String) -> coverage_primary_file.Box
// PRIMARY: sil_coverage_map {{.*}} // coverage_primary_file.main() -> ()
// PRIMARY-NOT: sil_coverage_map

extension Box {
  init(y: Int) { self.init() }
}

extension Box {
  init(z: String) { self.init() }
}

func main() {
  var b = Box()
  let _ = b.x
}
