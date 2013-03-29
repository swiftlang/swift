// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -sil-irgen -i %s | FileCheck %s
// REQUIRES: sdk

import Darwin

// CHECK: Hello world
puts("Hello world")
