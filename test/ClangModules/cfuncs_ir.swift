// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm -o - | FileCheck %s

import cfuncs

// CHECK: call void @exit(i32 17)
exit(17)
