// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_decls -primary-file %s %S/Inputs/coverage_imports.swift

func main() {
  var b = Box()
  let _ = b.x
}
