// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_decls %s %S/Inputs/coverage_imports.swift | %FileCheck %s -check-prefix=ALL
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_decls -primary-file %s %S/Inputs/coverage_imports.swift | %FileCheck %s -check-prefix=PRIMARY

// ALL: sil_coverage_map {{.*}} // closure #1 () -> Swift.Int in coverage_decls.Box.x.getter : Swift.Int
// ALL: sil_coverage_map {{.*}} // coverage_decls.Box.init(y: Swift.Int) -> coverage_decls.Box
// ALL: sil_coverage_map {{.*}} // coverage_decls.Box.init(z: Swift.String) -> coverage_decls.Box
// ALL: sil_coverage_map {{.*}} // coverage_decls.main() -> ()
// ALL: sil_coverage_map {{.*}} // __ntd_Box

// PRIMARY-NOT: sil_coverage_map
// PRIMARY: sil_coverage_map {{.*}} // coverage_decls.Box.init(y: Swift.Int) -> coverage_decls.Box
// PRIMARY: sil_coverage_map {{.*}} // coverage_decls.Box.init(z: Swift.String) -> coverage_decls.Box
// PRIMARY: sil_coverage_map {{.*}} // coverage_decls.main() -> ()
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
