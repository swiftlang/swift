// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_non_constructible_enum %s | %FileCheck %s

// Reduced from Carthage (https://github.com/Carthage/Carthage).
enum NoError: Error, Equatable {
// CHECK-LABEL: static coverage_non_constructible_enum.NoError.== infix
// CHECK: builtin "int_instrprof_increment"
// CHECK: unreachable

// CHECK-LABEL: sil_coverage_map {{.*}} static coverage_non_constructible_enum.NoError.== infix
// CHECK-NEXT: [[@LINE+1]]:54 -> [[@LINE+3]]:4 : 0
  static func ==(lhs: NoError, rhs: NoError) -> Bool {
    return true
  }
}
