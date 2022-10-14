// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -emit-sorted-sil -module-name coverage_maindecl -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir -parse-as-library %s

// CHECK-NOT: sil_coverage_map {{.*}} "main"

@main
struct S {
  // CHECK:        sil_coverage_map {{.*}} "{{.*}}s17coverage_maindecl1SV4mainyyFZ"
  // CHECK-NEXT:   [[@LINE+1]]:22 -> [[@LINE+10]]:4 : 0
  static func main() {
    var i : Int32 = 0

    // CHECK-NEXT: [[@LINE+3]]:11 -> [[@LINE+3]]:19 : (0 + 1)
    // CHECK-NEXT: [[@LINE+2]]:20 -> [[@LINE+4]]:6 : 1
    // CHECK-NEXT: [[@LINE+3]]:6 -> [[@LINE+4]]:4 : 0
    while (i < 10) {
      i += 1
    }
  } // CHECK-NEXT: }
}

// CHECK-NOT: sil_coverage_map {{.*}} "main"
