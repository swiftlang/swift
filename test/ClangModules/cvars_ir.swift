// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm -o - | FileCheck %s
// RUN: ls -lR %t/clang-module-cache | grep cvars.pcm

import cvars

// Check that the mangling is correct.
// CHECK: @PI = external global [[FLOAT:%.*]]

func getPI() -> Float {
  return PI
}
