// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=/ %s
// RUN: ls -lR %t/clang-module-cache | grep _Builtin_intrinsics.pcm
import _Builtin_intrinsics
