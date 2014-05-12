// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s -emit-ir -o - | FileCheck %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s --check-prefix=CHECK-LS
// CHECK-LS: cvars{{.*}}.pcm

import cvars

// Check that the mangling is correct.
// CHECK: @PI = external global [[FLOAT:%.*]]

func getPI() -> Float {
  return PI
}
