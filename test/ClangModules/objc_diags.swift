// RUN: rm -rf %t/clang-module-cache
// RUN: not %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s -debug-constraints 2> %t.out
// RUN: FileCheck %s < %t.out

import ObjectiveC

func instanceMethod(b: B) {
  // CHECK: found solution
  // CHECK: found solution
  // CHECK: found solution
  // CHECK: found solution
  // CHECK: found solution
  // FIXME: improve this diagnostic
  b.method(1, 2.5)
}
