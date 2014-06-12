// RUN: rm -rf %t/clang-module-cache
// RUN: %target-build-swift -parse -Xfrontend -verify -module-cache-path %t/clang-module-cache -sdk %sdk -target %target-triple %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: _Builtin_intrinsics{{.*}}.pcm
// REQUIRES: sdk
import _Builtin_intrinsics
