// Note: this test intentionally uses a private module cache.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -module-cache-path %t/clang-module-cache -I %S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s

// XFAIL: linux

// CHECK: cfuncs{{.*}}.pcm

import cfuncs

func test_puts(s: String) {
  _ = puts(s) + 32
}
