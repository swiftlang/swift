// Note: this test intentionally uses a private module cache.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -module-cache-path %t/clang-module-cache -I %S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | %FileCheck %s

// CHECK: cfuncs{{.*}}.pcm

import cfuncs

func test_puts(_ s: String) {
  _ = puts(s) + (32 as Int32)
}
