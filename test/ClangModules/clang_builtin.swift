// Note: this test intentionally uses a private module cache.
//
// RUN: rm -rf %t/clang-module-cache
// RUN: %target-swift-frontend -parse -verify -module-cache-path %t/clang-module-cache %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: _Builtin_intrinsics{{.*}}.pcm
// REQUIRES: sdk
import _Builtin_intrinsics
