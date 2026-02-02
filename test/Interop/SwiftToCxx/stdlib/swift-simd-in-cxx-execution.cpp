// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-simd-in-cxx.swift -module-name SIMD -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/simd.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-simd-execution.o
// RUN: %target-interop-build-swift %S/swift-simd-in-cxx.swift -o %t/swift-simd-execution -Xlinker %t/swift-simd-execution.o -module-name SIMD -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-simd-execution
// RUN: %target-run %t/swift-simd-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: rdar157848231

#include "simd.h"
#include <assert.h>
#include <iostream>

int main() {
  swift_float3 vec{1.0, 2.0, 3.0};
  SIMD::swiftThingSIMD(vec);
  // CHECK: SIMD3<Float>(1.0, 2.0, 3.0)
  SIMD::swiftThingSIMD2(vec);
  // CHECK: SIMD3<Float>(1.0, 2.0, 3.0)
  swift_float3 vec2 = SIMD::swiftThingSIMD3();
  std::cout << vec2.x << " " << vec2.y << " " << vec2.z << "\n";
  // CHECK: 4 5 6
  vec2 = SIMD::swiftThingSIMD4();
  std::cout << vec2.x << " " << vec2.y << " " << vec2.z << "\n";
  // CHECK: 4 5 6
  return 0;
}

