// RUN: not %swift %clang-importer-sdk -parse -target x86_64-apple-macosx10.9 %s -debug-constraints 2> %t.out
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
