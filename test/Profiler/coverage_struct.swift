// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_struct %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

struct Foo {
  var a = false

// CHECK: sil_coverage_map {{.*}}// variable initialization expression of coverage_struct.Foo.b : Swift.Bool
// CHECK-NEXT: [[@LINE+1]]:11 -> [[@LINE+3]]:6 : 0
  let b = {
    false
  }()
}
