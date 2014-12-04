// Note: this test intentionally uses a private module cache.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -I %S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: cfuncs{{.*}}.pcm

import cfuncs

func test_puts(s: String) {
  var i = puts(s) + 32
}
