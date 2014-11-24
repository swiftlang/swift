// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: cvars{{.*}}.pcm

import cvars

func getPI() -> Float {
  return PI
}

func testPointers() {
  let cp = globalConstPointer
  cp.abcde() // expected-error {{'UnsafePointer<Void>' does not have a member named 'abcde'}}
  let mp = globalPointer
  mp.abcde() // expected-error {{'UnsafeMutablePointer<Void>' does not have a member named 'abcde'}}
}
