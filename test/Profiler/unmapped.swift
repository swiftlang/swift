// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name unmapped %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// This test is exclusively for AST that we should never profile, as there is
// no interesting user-written code.

// CHECK-NOT: increment_profiler_counter
// CHECK-NOT: sil_coverage_map

struct S {
  // Don't profile the implicit accessor, or the implicit constructor.
  var x: Int
}

// Don't profile any synthesized codable methods.
struct R : Codable {
  var x: String
  var y: Int
}

// Don't profile the implicit rawValue.
enum E : Int {
  case a
}
