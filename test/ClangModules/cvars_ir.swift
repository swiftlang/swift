// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 %s -emit-ir -o - | FileCheck %s

import cvars

// Check that the mangling is correct.
// CHECK: @PI = external global [[FLOAT:%.*]]

func getPI() -> Float {
  return PI
}
