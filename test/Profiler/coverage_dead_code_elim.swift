// RUN: %target-swift-frontend %s -profile-generate -profile-coverage-mapping  -O -whole-module-optimization -emit-ir -o - | %FileCheck %s

// CHECK-NOT: llvm_coverage_mapping = internal constant

func foo() {}
