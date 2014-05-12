// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: cvars{{.*}}.pcm

import cvars

func getPI() -> Float {
  return PI
}
