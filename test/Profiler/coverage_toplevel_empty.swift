// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -emit-sorted-sil -module-name coverage_toplevel_empty %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// Make sure we don't emit a coverage map for an empty main file with no
// top-level code.
// CHECK-NOT: sil_coverage_map
