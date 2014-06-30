// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %clang-importer-sdk -enable-string-pointer-conversion -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: cfuncs{{.*}}.pcm

import cfuncs

func test_puts(s: String) {
  var i = puts(s) + 32
}
